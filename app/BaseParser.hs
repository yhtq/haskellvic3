{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module BaseParser where
import Prelude hiding (exp)
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Data.Text (Text, pack)
import Data.Map (Map) 
import Control.Monad.Identity (Identity, liftM)
import Text.Parsec.Token (GenTokenParser(stringLiteral))
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
lexer :: GenTokenParser Text () Identity
lexer = Tok.makeTokenParser paradoxLanguage
lexeme :: ParsecT Text () Identity a -> ParsecT Text () Identity a
lexeme = Tok.lexeme lexer
symbol :: String -> ParsecT Text () Identity String
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
parseVar :: ParadoxParser Var 
parseVar = do
    name <- parseIdentifier
    return $ Var name


    
    
