{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module TestParse() where
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
    Tok.commentStart = "#",
    Tok.commentEnd = "",
    Tok.commentLine = "#",
    Tok.nestedComments = True,
    Tok.identStart = letter <|> char '@',
    Tok.identLetter = alphaNum ,
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
data Var = Var {
    var_name :: Key,
    scope :: Text
}
data Color = Color {
    colorType :: Text,
    colorValue :: [ValueFloat]
} deriving (Show)
type ValueInt = Integer
type ValueFloat = Double 
data ValueOp = Add | Multiply | Subtract | Divide | Min | Max deriving (Show)
opMap :: String -> ValueOp
opMap "add" = Add
opMap "multiply" = Multiply
opMap "subtract" = Subtract
opMap "divide" = Divide
data ValueExp a = ValueExpWithDesc Text (ValueExp a) | 
                    RawStaticalValue a | 
                    RawKeyValue Key | 
                    RawScriptedValue Text |
                    Exp ValueOp (ValueExp a) (ValueExp a) |
                    IfExp BoolExp (IfStructure a) |
                    AppendIfExp (ValueExp a) (IfStructure (ValueExpCal a))
                    deriving (Show)
newtype ValueExpCal a = ValueExpCal (ValueExp a -> ValueExp a)
instance (Show a, Num a) => Show (ValueExpCal a) where
    show :: (Show a, Num a) => ValueExpCal a -> String
    show (ValueExpCal f) = showWithOutValue (f (RawStaticalValue 0))
showWithOutValue :: (Show a, Num a) => ValueExp a -> String
showWithOutValue (ValueExpWithDesc desc exp) = ""
data IfStructure a = If BoolExp a  |
                     IfElse BoolExp a a |
                     IfElseIf BoolExp a (IfStructure a) 
                     deriving (Show)
type ValueIntExp = ValueExp ValueInt
type ValueFloatExp = ValueExp ValueFloat
class ValueNum a where
    parseNum :: ParadoxParser a
    -- toValueExp :: a -> ValueExp a
    -- toValueExp = RawStaticalValue
    toExp :: ValueExp a -> Exp
    defaultValue :: a
instance ValueNum ValueInt where
    parseNum = parseInt
    toExp = FromIntExp
    defaultValue = 0 
instance ValueNum ValueFloat where
    parseNum = parseFloat
    toExp = FromFloatExp
    defaultValue = 0.0
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
data Exp = FromIntExp ValueIntExp | FromFloatExp ValueFloatExp | FromBoolExp BoolExp | FromSwitchExp Switch | FromVarExp Key | FromColor Color | FromText Text deriving (Show)
-- data TypedExp = TypedExp {
--     typedExp :: Exp,
--     typedExpType :: Text
-- } deriving (Show)
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

-- parser
lexer = Tok.makeTokenParser paradoxLanguage
lexeme = Tok.lexeme lexer
symbol = Tok.symbol lexer
convertToText :: (Monad m) => (a -> m String) -> (a -> m Text)
convertToText f = fmap pack.f

parseIntWithSpaceLeft :: ParadoxParser ValueInt
parseIntWithSpaceLeft = do
    sign <- option 1 (char '-' >> return (-1))
    n <- Tok.integer lexer
    return (sign * n)
parseInt :: ParadoxParser ValueInt
parseInt = lexeme parseIntWithSpaceLeft
parseFloatWithSpaceLeft :: ParadoxParser ValueFloat
parseFloatWithSpaceLeft = do
    sign <- option 1 (char '-' >> return (-1))
    n <- try (Tok.float lexer) <|> fmap fromIntegral (Tok.integer lexer)
    return (sign * n)
parseFloat :: ParadoxParser ValueFloat
parseFloat = lexeme parseFloatWithSpaceLeft
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
parseObjcet :: ParadoxParser Object
parseObjcet = do
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
parseAssignment :: ParadoxParser Assignment
parseAssignment = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- parseExp
    return $ Assignment key value
parseExp :: ParadoxParser Exp
parseExp = do
    parseExp <|> parseRawStaticalValue
parseRawStaticalValue :: ParadoxParser Exp
parseRawStaticalValue = do
    (fmap FromIntExp parseValueIntExp) 
    <|>
    (fmap FromFloatExp parseValueFloatExp) 
    -- <|> parseBoolExp <|> parseVarExp <|> parseColorExp <|> parseText
parseExpBlock :: ParadoxParser Exp
parseExpBlock = do
    parseReserved "{"
    exp <- parseRawStaticalValue
    parseReserved "}"
    return exp
-- ValueNumExp : ValueNumRaw | ValueNumExpBlock 
-- ValueNumExpBlock : "{" ValueNumExp' "}"
-- ValueNumExp' : "value"" "=" ValueNumExp | ε | ValueNumExp' ValueNumOp "=" ValueNumExp
-- chainl 函数可以直接解析左递归的表达式，太好用了 
parseValueIntExp :: ParadoxParser ValueIntExp
parseValueIntExp = parseValueNumExp :: ParadoxParser ValueIntExp
parseValueFloatExp :: ParadoxParser ValueFloatExp
parseValueFloatExp = parseValueNumExp :: ParadoxParser ValueFloatExp
parseValueNumExp :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumRaw = do
    fmap RawStaticalValue parseNum <|> fmap RawKeyValue parseIdentifier <|> fmap RawScriptedValue parseText
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

    
    
