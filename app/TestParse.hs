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
type ParadoxMetaLanguage = Tok.GenLanguageDef Text () Identity
type ParadoxMetaParser = Parsec Text () 
paradoxMetaLanguage :: ParadoxMetaLanguage
paradoxMetaLanguage = emptyDef {
    Tok.commentStart = "#",
    Tok.commentEnd = "",
    Tok.commentLine = "#",
    Tok.nestedComments = True,
    Tok.identStart = letter ,
    Tok.identLetter = alphaNum ,
    Tok.opStart = oneOf "<>=",
    Tok.opLetter = oneOf "<>=",
    Tok.reservedNames = ["if", "else", "else_if", "limit", "switch", "while", "yes", "no", "AND", "OR", "NOT", "value", "add", "multiply", "subtract", "divide", "desc", "{", "}"] ,
    Tok.reservedOpNames = ["<=", ">=", "<", ">", "="],
    Tok.caseSensitive = True

}
type ScopeMap = Map Text Text
type Undetermined = ()
type Key = Text
type Query = Text
type Desc = Text
data Var = Var {
    var_name :: Key,
    scope :: Text
}
type ValueInt = Integer
type ValueFloat = Double 
data ValueOp = Add | Multiply | Subtract | Divide  deriving (Show)
opMap :: String -> ValueOp
opMap "add" = Add
opMap "multiply" = Multiply
opMap "subtract" = Subtract
opMap "divide" = Divide
data ValueExp a = ValueExpWithDesc Text (ValueExp a) | 
                    ExpRaw a | 
                    RawVar Key | 
                    Exp ValueOp (ValueExp a) (ValueExp a) |
                    IfExp BoolExp (IfStructure a) |
                    AppendIfExp (ValueExp a) (IfStructure (ValueExpCal a))
                    deriving (Show)
newtype ValueExpCal a = ValueExpCal (ValueExp a -> ValueExp a)
instance (Show a, Num a) => Show (ValueExpCal a) where
    show :: (Show a, Num a) => ValueExpCal a -> String
    show (ValueExpCal f) = showWithOutValue (f (ExpRaw 0))
showWithOutValue :: (Show a, Num a) => ValueExp a -> String
showWithOutValue (ValueExpWithDesc desc exp) = ""
data IfStructure a = If BoolExp a  |
                     IfElse BoolExp a a |
                     IfElseIf BoolExp a (IfStructure a) 
                     deriving (Show)
type ValueIntExp = ValueExp ValueInt
type ValueFloatExp = ValueExp ValueFloat
class ValueNum a where
    parseNum :: ParadoxMetaParser a
    toValueExp :: a -> ValueExp a
    toValueExp = ExpRaw
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
data Color = Color {
    colorType :: Text,
    colorValue :: [ValueFloat]
} deriving (Show)
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
    assignments :: [Assignment],
    properties :: ScopeMap
} deriving (Show)
lexer = Tok.makeTokenParser paradoxMetaLanguage
lexeme = Tok.lexeme lexer
parseInt :: ParadoxMetaParser ValueInt
parseInt = lexeme $ do
    sign <- option 1 (char '-' >> return (-1))
    n <- Tok.integer lexer
    return (sign * n)
parseFloat :: ParadoxMetaParser ValueFloat
parseFloat = lexeme $ do
    sign <- option 1 (char '-' >> return (-1))
    n <- Tok.float lexer
    return (sign * n)
parseIdentifier :: ParadoxMetaParser Text
parseIdentifier = lexeme $
    fmap pack (Tok.identifier lexer)
parseReserved :: String -> ParadoxMetaParser ()
parseReserved = lexeme.Tok.reserved lexer
parseReservedOp :: String -> ParadoxMetaParser ()
parseReservedOp = lexeme.Tok.reservedOp lexer
parseText :: ParadoxMetaParser Text
parseText = lexeme $ fmap pack (stringLiteral lexer)
parseObjcet :: ParadoxMetaParser Object
parseObjcet = do
    name <- parseIdentifier
    parseReservedOp "="
    parseReserved "{"
    assignments <- many parseAssignment
    parseReserved "}"
    return $ Object name assignments Map.empty
parseAssignment :: ParadoxMetaParser Assignment
parseAssignment = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- parseExp
    return $ Assignment key value
parseExp :: ParadoxMetaParser Exp
parseExp = do
    parseExp <|> parseExpRaw
parseExpRaw :: ParadoxMetaParser Exp
parseExpRaw = do
    (fmap FromIntExp parseValueIntExp) 
    <|>
    (fmap FromFloatExp parseValueFloatExp) 
    -- <|> parseBoolExp <|> parseVarExp <|> parseColorExp <|> parseText
parseExpBlock :: ParadoxMetaParser Exp
parseExpBlock = do
    parseReserved "{"
    exp <- parseExpRaw
    parseReserved "}"
    return exp
-- ValueNumExp : ValueNumRaw | ValueNumExpBlock 
-- ValueNumExpBlock : "{" ValueNumExp' "}"
-- ValueNumExp' : "value"" "=" ValueNumExp | ε | ValueNumExp' ValueNumOp "=" ValueNumExp
-- chainl 函数可以直接解析左递归的表达式，太好用了 
parseValueIntExp :: ParadoxMetaParser ValueIntExp
parseValueIntExp = parseValueNumExp :: ParadoxMetaParser ValueIntExp
parseValueFloatExp :: ParadoxMetaParser ValueFloatExp
parseValueFloatExp = parseValueNumExp :: ParadoxMetaParser ValueFloatExp
parseValueNumExp :: (ValueNum a) => ParadoxMetaParser (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a) => ParadoxMetaParser (ValueExp a)
parseValueNumRaw = do
    fmap toValueExp parseNum <|> fmap RawVar parseIdentifier
parseValueNumExpBlock :: (ValueNum a) => ParadoxMetaParser (ValueExp a)
parseValueNumExpBlock = do
    parseReserved "{"
    exp <- parseValueNumExp'
    parseReserved "}"
    return exp
parseAppendValueNumExp :: (ValueNum a) => ParadoxMetaParser (ValueExp a -> ValueExp a)
parseAppendValueNumExp = do
                            let append' op = do 
                                        parseReservedOp "="
                                        exp <- parseValueNumExp
                                        return (\baseExp -> Exp op baseExp exp, pack "")
                            let append = (do {parseReservedOp "add"; append' Add})
                                    <|> (do {parseReservedOp "multiply"; append' Multiply})
                                    <|> (do {parseReservedOp "subtract"; append' Subtract})
                                    <|> (do {parseReservedOp "divide"; append' Divide})
                                    <|> (do 
                                            parseReserved "desc"
                                            parseReservedOp "="
                                            desc <- parseText
                                            return (id, desc)
                                        )
                            let concatWithDesc (f1, desc1) (f2, desc2) = (f1 . f2, desc1 <> desc2)
                            (f, desc) <- chainl append (return concatWithDesc) (id, pack "")
                            if (DT.null desc) then return f else return (ValueExpWithDesc desc . f)


parseValueNumExp' :: (ValueNum a) => ParadoxMetaParser (ValueExp a)
parseValueNumExp' = do
                        base <- option (toValueExp defaultValue ) (
                                do
                                    parseReserved "value"
                                    parseReservedOp "="
                                    parseValueNumExp
                                )
                        f <- parseAppendValueNumExp
                        return $ f base

    
    
