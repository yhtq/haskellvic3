{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module TestParse where
import Prelude hiding (exp)
import Text.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Data.Text (Text, pack)
import qualified Data.Text as DT
import Data.Map (Map) 
import qualified Data.Map as Map
import Control.Monad.Identity (Identity, liftM)
import GHC.TypeLits (ErrorMessage(Text))
import Text.Parsec.Token (GenTokenParser(stringLiteral))
import Data.Map.Strict (fromListWithKey)
type ParadoxLanguage = Tok.GenLanguageDef Text () Identity
type ParadoxParser = Parsec Text () 
paradoxLanguage :: ParadoxLanguage
paradoxLanguage = emptyDef {
    Tok.commentStart = "",
    Tok.commentEnd = "",
    Tok.commentLine = "#",
    Tok.nestedComments = True,
    Tok.identStart = letter <|> char '@',
    Tok.identLetter = alphaNum <|> char '_' ,
    Tok.opStart = oneOf "<>=?",
    Tok.opLetter = oneOf "<>=",
    Tok.reservedNames = ["if", "else", "else_if", "limit", "switch", "while", 
        "yes", "no", 
        "AND", "OR", "NOT", 
        "value", "add", "multiply", "subtract", "divide", "min", "max",
        "desc", 
        "{", "}",
        "hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360",
        "@["] ,
    Tok.reservedOpNames = ["<=", ">=", "<", ">", "==", "?=", "!=", "="],
    Tok.caseSensitive = True

}

-- 类型定义
type ScopeMap = Map Text Text
type DefinitionMap = Map Text Exp
type Undetermined = ()
type Key = Text
type Query = Text
type Desc = Text
newtype Var = Var Key deriving (Show)
data Color = Color {
    colorType :: Text,
    colorValue :: [ValueFloat]
} deriving (Show)
type ValueInt = Integer
type ValueFloat = Double 
data ValueUntypedNum = ValueInt ValueInt | ValueFloat ValueFloat deriving (Show)
instance Num ValueUntypedNum where 
    (+) (ValueInt a) (ValueInt b) = ValueInt (a + b)
    (+) (ValueFloat a) (ValueFloat b) = ValueFloat (a + b)
    (+) (ValueInt a) (ValueFloat b) = ValueFloat (fromIntegral a + b)
    (+) (ValueFloat a) (ValueInt b) = ValueFloat (a + fromIntegral b)
    (*) (ValueInt a) (ValueInt b) = ValueInt (a * b)
    (*) (ValueFloat a) (ValueFloat b) = ValueFloat (a * b)
    (*) (ValueInt a) (ValueFloat b) = ValueFloat (fromIntegral a * b)
    (*) (ValueFloat a) (ValueInt b) = ValueFloat (a * fromIntegral b)
    (-) (ValueInt a) (ValueInt b) = ValueInt (a - b)
    (-) (ValueFloat a) (ValueFloat b) = ValueFloat (a - b)
    (-) (ValueInt a) (ValueFloat b) = ValueFloat (fromIntegral a - b)
    (-) (ValueFloat a) (ValueInt b) = ValueFloat (a - fromIntegral b)
    abs (ValueInt a) = ValueInt (abs a)
    abs (ValueFloat a) = ValueFloat (abs a)
    signum (ValueInt a) = ValueInt (signum a)
    signum (ValueFloat a) = ValueFloat (signum a)
    fromInteger a = ValueInt a
data ValueOp = Add | Multiply | Subtract | Divide | Min | Max deriving (Show)
opMap :: String -> ValueOp
opMap "add" = Add
opMap "multiply" = Multiply
opMap "subtract" = Subtract
opMap "divide" = Divide
opMap "min" = Min
opMap "max" = Max
opMap _ = undefined
data ValueExp a = ValueExpWithDesc Text (ValueExp a) | 
                    RawStaticalValue a | 
                    RawVar Var | 
                    RawScriptedValue Text |
                    Exp ValueOp (ValueExp a) (ValueExp a) |
                    IfExp BoolExp (IfStructure a) |
                    AppendIfExp (ValueExp a) (IfStructure (ValueExpCal a))
                    deriving (Show)
newtype ValueExpCal a = ValueExpCal (ValueExp a -> ValueExp a)
instance (Show a, Num a, ValueNum a) => Show (ValueExpCal a) where
    show :: (Show a, Num a) => ValueExpCal a -> String
    show (ValueExpCal f) = showWithOutValue (f $ RawStaticalValue defaultValue)
showWithOutValue :: (Show a, Num a) => ValueExp a -> String
showWithOutValue (ValueExpWithDesc desc exp) = ""


data IfStructure a = If BoolExp a  |
                     IfElse BoolExp a a |
                     IfElseIf BoolExp a (IfStructure a) 
                     deriving (Show)
type ValueIntExp = ValueExp ValueInt
type ValueFloatExp = ValueExp ValueFloat
type ValueUntypedNumExp = ValueExp ValueUntypedNum


class ValueNum a where
    parseNum :: ParadoxParser a
    -- toValueExp :: a -> ValueExp a
    -- toValueExp = RawStaticalValue
    defaultValue :: a
instance ValueNum ValueInt where
    parseNum = parseInt
    defaultValue = 0 
instance ValueNum ValueFloat where
    parseNum = parseFloat
    defaultValue = 0.0
instance ValueNum ValueUntypedNum where
    parseNum = parseUntypedNum
    defaultValue = ValueInt 0


data ValueBool = Yes | No deriving (Show, Eq)
data BoolOp = And | Or deriving (Show)
data CmpOp = Less | Greater | LessEq | GreaterEq deriving (Show)
data BoolNot = Not deriving (Show)
data BoolExp = BoolExp BoolOp BoolExp BoolExp | 
                BoolExp' BoolNot BoolExp | 
                IntCmp CmpOp ValueIntExp ValueIntExp |
                FloatCmp CmpOp ValueFloatExp ValueFloatExp |
                Q Query Key |
                BoolRaw ValueBool 
                deriving (Show)

data Exp = FromUntypedNumExp ValueUntypedNumExp | FromIntExp ValueIntExp | FromFloatExp ValueFloatExp | FromBoolExp BoolExp | FromSwitchExp Switch | FromVarExp Var | FromColor Color | FromText Text deriving (Show)
class Term a where 
    toExp :: a -> Exp
instance Term ValueIntExp where
    toExp = FromIntExp
instance Term ValueFloatExp where
    toExp = FromFloatExp
instance Term ValueUntypedNumExp where
    toExp = FromUntypedNumExp
instance Term BoolExp where
    toExp = FromBoolExp
instance Term Switch where
    toExp = FromSwitchExp
instance Term Color where
    toExp = FromColor
instance Term Text where
    toExp = FromText 
instance Term Var where
    toExp = FromVarExp


type Limit = BoolExp
data Switch = Switch {
    switch :: Key,
    cases :: [Exp]
} deriving (Show) 
data Assignment = Assignment {
    key :: Key,
    value :: Exp
} deriving (Show)
data Object = Object {
    obj_name :: Text,
    assignments :: DefinitionMap   -- 定义object时给出的属性
} deriving (Show)
liftToExp :: (Monad m, Term a) => m a -> m Exp 
liftToExp = liftM toExp

-- parser
lexer = Tok.makeTokenParser paradoxLanguage
lexeme = Tok.lexeme lexer
symbol = Tok.symbol lexer
convertToText :: (Monad m) => (a -> m String) -> (a -> m Text)
convertToText f = fmap pack.f

parseIntWithSpaceLeft :: ParadoxParser ValueInt
parseIntWithSpaceLeft = Tok.integer lexer
parseInt :: ParadoxParser ValueInt
parseInt = lexeme parseIntWithSpaceLeft
parseFloatWithSpaceLeft :: ParadoxParser ValueFloat
parseFloatWithSpaceLeft = try (Tok.float lexer) <|> fmap fromIntegral parseIntWithSpaceLeft
parseFloat :: ParadoxParser ValueFloat
parseFloat = lexeme parseFloatWithSpaceLeft
parseUntypedNum :: ParadoxParser ValueUntypedNum
parseUntypedNum = try (fmap ValueFloat $ Tok.float lexer) <|> fmap ValueInt parseInt
parseWhiteSpaces :: ParadoxParser ()
parseWhiteSpaces = Tok.whiteSpace lexer
parseIdentifier :: ParadoxParser Text
parseIdentifier = lexeme $
    fmap pack (Tok.identifier lexer)
parseReserved :: String -> ParadoxParser ()
parseReserved = lexeme.Tok.reserved lexer
parseReservedWithReturn :: String -> ParadoxParser Text
parseReservedWithReturn str = do
    parseReserved str
    return $ pack str
parseReservedOp :: String -> ParadoxParser ()
parseReservedOp = lexeme.Tok.reservedOp lexer
parseText :: ParadoxParser Text
parseText = lexeme $ fmap pack (stringLiteral lexer)
parseColor :: ParadoxParser Color
parseColor = do
    colorType <- choice $ map parseReservedWithReturn ["hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360"]
    parseReservedOp "{"
    colorValue <- sepBy parseFloatWithSpaceLeft parseWhiteSpaces
    parseReservedOp "}"
    return $ Color colorType colorValue
parseObject :: ParadoxParser Object
parseObject = do
    parseWhiteSpaces
    name <- parseIdentifier
    parseReservedOp "="
    parseReserved "{"
    assignments <- many parseAssignment
    parseReserved "}"
    let buildMap f = fromListWithKey f.map (\(Assignment k v) -> (k, return v :: ParadoxParser Exp))
    let errorReport k _ _ = fail $ "duplicate key: " ++ DT.unpack k
    let assignmentsMapParse = sequence $ buildMap errorReport assignments
    assignmentsMap <- assignmentsMapParse
    return $ Object name assignmentsMap
parseObjects :: ParadoxParser [Object]
parseObjects = do
    parseWhiteSpaces
    many1 parseObject
parseAssignment :: ParadoxParser Assignment
parseAssignment = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- parseExp
    return $ Assignment key value
parseExp :: ParadoxParser Exp
parseExp = do
    parseExpBlock 
    <|> parseValueExp 
    -- 暂且统一视作Value?
    -- <|> liftToExp parseVar 
    <|> liftToExp parseText
    <|> liftToExp parseColor
    <|> liftToExp parseIdentifier
    -- parseBoolExp <|> parseColorExp 
parseVar :: ParadoxParser Var 
parseVar = do
    name <- parseIdentifier
    return $ Var name

-- 或许可以优化，解析整数失败不一定要回溯
parseValueExp :: ParadoxParser Exp
parseValueExp = do
    liftToExp $ try parseUntypedNumExp
    -- liftToExp $ try parseValueIntExp 
    -- <|>
    -- liftToExp parseValueFloatExp 
    -- <|> parseBoolExp <|> parseVarExp <|> parseColorExp <|> parseText
parseExpBlock :: ParadoxParser Exp
parseExpBlock = Tok.braces lexer parseExp
-- ValueNumExp : ValueNumRaw | ValueNumExpBlock 
-- ValueNumExpBlock : "{" ValueNumExp' "}"
-- ValueNumExp' : "value"" "=" ValueNumExp | ε | ValueNumExp' ValueNumOp("add"...) "=" ValueNumExp
-- chainl 函数可以直接解析左递归的表达式
parseValueIntExp :: ParadoxParser ValueIntExp
parseValueIntExp = parseValueNumExp :: ParadoxParser ValueIntExp
parseValueFloatExp :: ParadoxParser ValueFloatExp
parseValueFloatExp = parseValueNumExp :: ParadoxParser ValueFloatExp
parseUntypedNumExp :: ParadoxParser ValueUntypedNumExp
parseUntypedNumExp = parseValueNumExp :: ParadoxParser ValueUntypedNumExp
parseValueNumExp :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumRaw = do
    fmap RawStaticalValue parseNum <|> fmap (RawVar . Var) parseIdentifier <|> fmap RawScriptedValue parseText
parseValueNumExpBlock :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExpBlock = do
    parseReserved "{"
    exp <- parseValueNumExp'
    parseReserved "}"
    return exp
parseAppendValueNumExp :: (ValueNum a) => ParadoxParser (ValueExp a -> ValueExp a)
parseAppendValueNumExp = do
                            let append' op = do 
                                        parseReservedOp "="
                                        exp <- parseValueNumExp
                                        return (\baseExp -> Exp op baseExp exp, pack "")
                            let append = (do {parseReservedOp "add"; append' Add})
                                    <|> (do {parseReservedOp "multiply"; append' Multiply})
                                    <|> (do {parseReservedOp "subtract"; append' Subtract})
                                    <|> (do {parseReservedOp "divide"; append' Divide})
                                    <|> (do {parseReservedOp "min"; append' Min})
                                    <|> (do {parseReservedOp "max"; append' Max})
                                    <|> (do 
                                            parseReserved "desc"
                                            parseReservedOp "="
                                            desc <- parseText
                                            return (id, desc)
                                        )
                            let concatWithDesc (f1, desc1) (f2, desc2) = (f1 . f2, desc1 <> desc2)
                            (f, desc) <- chainl append (return concatWithDesc) (id, pack "")
                            if (DT.null desc) then return f else return (ValueExpWithDesc desc . f)


parseValueNumExp' :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExp' = do
                        base <- option (RawStaticalValue defaultValue ) (
                                do
                                    parseReserved "value"
                                    parseReservedOp "="
                                    parseValueNumExp
                                )
                        f <- parseAppendValueNumExp
                        return $ f base
runTestParser :: ParadoxParser a -> Text -> Either ParseError a
runTestParser p = parse p ""

    
    
