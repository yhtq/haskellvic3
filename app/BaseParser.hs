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
data NumTypeFlag = Int | Float deriving (Show, Eq)
type ParadoxLanguage s = Tok.GenLanguageDef Text s Identity
type StatedParadoxParser s = Parsec Text s
type ParadoxParser = StatedParadoxParser NumTypeFlag
paradoxLanguage :: ParadoxLanguage s
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
                    Exp (ValueExp a) (AppendingValueExp a) |
                    IfExp BoolExp (IfStructure a) |
                    AppendIfExp (ValueExp a) (IfStructure (AppendingValueExp a))
                    deriving (Show)
-- 第一个构造子是左结合的，换言之AppendingValueExp实际在其左侧
data AppendingValueExp a = ExpAppendings (AppendingValueExp a) ValueOp (ValueExp a)  | 
                        ExpAppending ValueOp (ValueExp a)  deriving (Show)
-- instance (Show a, Num a, ValueNum a) => Show (AppendingValueExp a) where
--     show :: (Show a, Num a) => AppendingValueExp a -> String
--     show f = showWithOutValue (f $ RawStaticalValue defaultValue)
-- showWithOutValue :: (Show a, Num a) => ValueExp a -> String
-- showWithOutValue (ValueExpWithDesc desc exp) = ""


data IfStructure a = If BoolExp a  |
                     IfElse BoolExp a a |
                     IfElseIf BoolExp a (IfStructure a) 
                     deriving (Show)

class ValueMap c where
    -- 两个参数分别是正向映射和反向映射
    valueMap :: (a -> b) -> c a -> c b
instance ValueMap AppendingValueExp where
    valueMap :: (a -> b) -> AppendingValueExp a -> AppendingValueExp b
    valueMap f (ExpAppendings b op a ) = ExpAppendings (valueMap f b) op (valueMap f a) 
    valueMap f (ExpAppending op a) = ExpAppending op (valueMap f a)
instance ValueMap IfStructure where 
    valueMap :: (a -> b) -> IfStructure a -> IfStructure b
    valueMap f (If exp a) = If exp (f a)
    valueMap f (IfElse exp a b) = IfElse exp (f a) (f b)
    valueMap f (IfElseIf exp a b) = IfElseIf exp (f a) (valueMap f b)
instance ValueMap ValueExp where
    valueMap :: (a -> b) -> ValueExp a -> ValueExp b
    valueMap f (ValueExpWithDesc desc exp) = ValueExpWithDesc desc (valueMap f exp)
    valueMap f (RawStaticalValue a) = RawStaticalValue (f a)
    valueMap f (RawVar var) = RawVar var
    valueMap f (RawScriptedValue script) = RawScriptedValue script
    valueMap f (Exp e ae) = Exp (valueMap f e) (valueMap f ae)
    valueMap f (IfExp exp ifStructure) = IfExp exp (valueMap f ifStructure)
    valueMap f (AppendIfExp a is) = AppendIfExp (valueMap f a) (valueMap (valueMap f) is)


type ValueIntExp = ValueExp ValueInt
type ValueFloatExp = ValueExp ValueFloat
type ValueUntypedNumExp = ValueExp ValueUntypedNum


class ValueNum a where
    parseNum :: StatedParadoxParser s a
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
lexer :: GenTokenParser Text s Identity
lexer = Tok.makeTokenParser paradoxLanguage
lexeme :: ParsecT Text s Identity a -> ParsecT Text s Identity a
lexeme = Tok.lexeme lexer
symbol :: String -> ParsecT Text s Identity String
symbol = Tok.symbol lexer
convertToText :: (Monad m) => (a -> m String) -> (a -> m Text)
convertToText f = fmap pack.f

parseIntWithSpaceLeft :: StatedParadoxParser s ValueInt
parseIntWithSpaceLeft = Tok.integer lexer
parseInt :: StatedParadoxParser s ValueInt
parseInt = lexeme parseIntWithSpaceLeft
parseFloatWithSpaceLeft :: StatedParadoxParser s ValueFloat
parseFloatWithSpaceLeft = try (Tok.float lexer) <|> fmap fromIntegral parseIntWithSpaceLeft
parseFloat :: StatedParadoxParser s ValueFloat
parseFloat = lexeme parseFloatWithSpaceLeft
parseUntypedNum :: StatedParadoxParser s ValueUntypedNum
parseUntypedNum = try (fmap ValueFloat $ Tok.float lexer) <|> fmap ValueInt parseInt
parseWhiteSpaces :: StatedParadoxParser s ()
parseWhiteSpaces = Tok.whiteSpace lexer
parseIdentifier :: StatedParadoxParser s Text
parseIdentifier = lexeme $
    fmap pack (Tok.identifier lexer)
parseReserved :: String -> StatedParadoxParser s ()
parseReserved = lexeme.Tok.reserved lexer
parseReservedWithReturn :: String -> StatedParadoxParser s Text
parseReservedWithReturn str = do
    parseReserved str
    return $ pack str
parseReservedOp :: String -> StatedParadoxParser s ()
parseReservedOp = lexeme.Tok.reservedOp lexer
parseText :: StatedParadoxParser s Text
parseText = lexeme $ fmap pack (stringLiteral lexer)
parseColor :: StatedParadoxParser s Color
parseColor = do
    colorType <- choice $ map parseReservedWithReturn ["hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360"]
    parseReservedOp "{"
    colorValue <- sepBy parseFloatWithSpaceLeft parseWhiteSpaces
    parseReservedOp "}"
    return $ Color colorType colorValue
parseVar :: StatedParadoxParser s Var 
parseVar = do
    name <- parseIdentifier
    return $ Var name


    
    
