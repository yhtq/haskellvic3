{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module BaseParser where
import Prelude hiding (exp)
-- import qualified Text.MegaParsec.Token as Tok
import  Text.Megaparsec.Char.Lexer  qualified as L
import Text.Megaparsec.Char as C
import Text.Megaparsec 
import Data.Text (Text, pack, unpack)
import Data.Map (Map) 
import Prettyprinter(Pretty(..))
import Data.String (IsString(..))
import Control.Monad.Identity (Identity)
import Control.Monad(liftM)
import Control.Monad.Trans.State.Lazy (StateT)
import Template ( expGen )
-- import Text.MegaParsec.Token (GenTokenParser(stringLiteral))
import Language.Haskell.TH (mkName)
data NumTypeFlag = Int | Float deriving (Show, Eq)
-- type ParadoxLanguage s = Tok.GenLanguageDef Text s Identity
type ParadoxParser = StateT () (Parsec () Text)
type StatedParadoxParser s = StateT s ParadoxParser

-- paradoxLanguage :: ParadoxLanguage s
-- paradoxLanguage = emptyDef {
--     Tok.commentStart = "",
--     Tok.commentEnd = "",
--     Tok.commentLine = "#",
--     Tok.nestedComments = True,
--     Tok.identStart = letter <|> char '@',
--     Tok.identLetter = alphaNum <|> char '_' ,
--     Tok.opStart = oneOf "<>=?",
--     Tok.opLetter = oneOf "<>=",
--     Tok.reservedNames = ["if", "else", "else_if", "limit", "switch", "while", 
--         "error_check", "severity",
--         "yes", "no", 
--         "AND", "OR", "NOT", "NOR", "NAND", 
--         "value", "add", "multiply", "subtract", "divide", "min", "max", "modulo",
--         "round", "ceiling", "floor", "round_to",
--         "desc", 
--         "{", "}",
--         "hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360",
--         ":",
--         "@["] ,
--     Tok.reservedOpNames = ["<=", ">=", "<", ">", "==", "?=", "!=", "="],
--     Tok.caseSensitive = True

-- }
reservedNames :: [Text]
reservedNames = ["if", "else", "else_if", "limit", "switch", "while", 
         "error_check", "severity",
         "yes", "no", 
         "AND", "OR", "NOT", "NOR", "NAND", 
         "value", "add", "multiply", "subtract", "divide", "min", "max", "modulo",
         "round", "ceiling", "floor", "round_to",
         "desc", 
         "{", "}",
         "hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360",
         ":",
         "@["] 
reservedChecker :: Text -> Bool
reservedChecker = (`elem` reservedNames)
reservedOpNames :: [Text]
reservedOpNames = ["<=", ">=", "<", ">", "==", "?=", "!=", "="]
reservedOpChecker :: Text -> Bool
reservedOpChecker = (`elem` reservedOpNames)
comment :: ParadoxParser ()
comment = L.skipLineComment ("#")
commentBlock :: ParadoxParser ()
commentBlock = L.skipBlockCommentNested ("/*") ("*/")
spaceConsumer :: ParadoxParser ()
spaceConsumer = C.space1
allSpace :: ParadoxParser ()
allSpace = L.space spaceConsumer comment commentBlock
lexeme :: ParadoxParser a -> ParadoxParser a
lexeme = L.lexeme allSpace

-- 类型定义
type Undetermined = ()
newtype Identifier = Text Text deriving (Show, Eq, Ord)
type Key = Identifier
instance IsString Identifier where
    fromString = Text . pack
instance Pretty Identifier where
    pretty (Text s) = pretty s
stringToIdentifier :: String -> Identifier
stringToIdentifier = Text . pack
textToIdentifier :: Text -> Identifier
textToIdentifier = Text
tokenToIdentifier :: Token Text -> Identifier
tokenToIdentifier = Text . pack . show

identifierToString :: Identifier -> String
identifierToString (Text s) = unpack s
identifierToText :: Identifier -> Text
identifierToText (Text s) = s
type Query = Var
type Scope = Identifier
data ScopeTransformer = ScopeTransformer {
    scopeScopeTransformer :: Scope,
    nameScopeTransformer :: Scope
} deriving (Show)
type Desc = Text
data Var = Var {
    name :: Identifier,
    varType :: Text
} deriving (Show, Eq)
data Color = Color {
    colorType :: Text,
    colorValue :: [ValueFloat]
} deriving (Show)
type Groups = [Identifier]
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
data ValueOp = Add | Multiply | Subtract | Divide 
                | Min | Max
                | Modulo
                | Round | Ceiling | Floor | RoundTo
                 deriving (Show)
opMap :: String -> ValueOp
opMap "add" = Add
opMap "multiply" = Multiply
opMap "subtract" = Subtract
opMap "divide" = Divide
opMap "min" = Min
opMap "max" = Max
opMap "modulo" = Modulo
opMap "round" = Round
opMap "ceiling" = Ceiling
opMap "floor" = Floor
opMap "round_to" = RoundTo
opMap _ = undefined

-- 在以下定义中，可以认为以Exp结尾的均为出现在 "=" 右侧的表达式
-- BoolExp 除外，它在一些特殊结构中以完整 "a = b" 形式出现
-- 在这里一般认为出现在顶层声明中 "=" 左端的是不做区分的 identifier
-- 这也是为什么将 possible 没有视作保留字但将其右侧单独处理
data ValueExp a = ValueExpWithDesc Text (ValueExp a) | 
                    RawStaticalValue a | 
                    -- RawVar Var |     疑似这里不能标注类型？
                    RawIdentifier Identifier |
                    RawScriptedValue Text |
                    Exp (ValueExp a) (AppendingValueExp a) |
                    IfExp BoolExp (IfStructure a) |
                    AppendIfExp (ValueExp a) (IfStructure (AppendingValueExp a))
                    deriving (Show)

-- 第一个构造子是左结合的，换言之AppendingValueExp实际在其左侧
data AppendingValueExp a = ExpAppendings (AppendingValueExp a) ValueOp (ValueExp a)  | 
                        ExpAppending ValueOp (ValueExp a)  deriving (Show)

data IfStructure a = If BoolExp a  |
                     IfElse BoolExp a a |
                     IfElseIf BoolExp a (IfStructure a) 
                     deriving (Show)

class ValueMap c where
    -- 类似Traversable的fmap
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
    valueMap _ (RawIdentifier name) = RawIdentifier name
    valueMap _ (RawScriptedValue script) = RawScriptedValue script
    valueMap f (Exp e ae) = Exp (valueMap f e) (valueMap f ae)
    valueMap f (IfExp exp ifStructure) = IfExp exp (valueMap f ifStructure)
    valueMap f (AppendIfExp a is) = AppendIfExp (valueMap f a) (valueMap (valueMap f) is)


type ValueIntExp = ValueExp ValueInt
type ValueFloatExp = ValueExp ValueFloat
type ValueUntypedNumExp = ValueExp ValueUntypedNum




data ValueBool = Yes | No deriving (Show, Eq)
data BoolOp = And | Or | Nor | NAND deriving (Show)
data CmpOp = Less | Greater | LessEq | GreaterEq | Eq | NotEq deriving (Show)
data BoolNot = Not deriving (Show)

-- 这里BoolExp是指出现在 limits 块中的完整表达式，ConstBoolExp 是出现在声明中的右值表达式
-- 注意默认情况下多个BoolExp是And的关系
data BoolExp =  AndList [BoolExp] |
                BoolOp BoolOp [BoolExp] | 
                BoolOp' BoolNot BoolExp | 
                IntCmp CmpOp ValueIntExp ValueIntExp |
                FloatCmp CmpOp ValueFloatExp ValueFloatExp |
                UntypedNumCmp CmpOp ValueUntypedNumExp ValueUntypedNumExp |
                Q Query Var |   -- query 指 "=" 左边的，key指 "=" 右边的
                ScopeTrans ScopeTransformer BoolExp 
                -- | BoolRaw ValueBool  似乎不允许这样写
                deriving (Show)
newtype ConstBoolExp = BoolRaw ValueBool deriving (Show)
data ErrorCheck = ErrorCheck {
    severity :: Identifier, 
    checkCondition :: BoolExp
} deriving (Show)
data PossibleExp = Possible BoolExp | WithErrorCheck [ErrorCheck] deriving (Show)


boolOpMap :: String -> BoolOp
boolOpMap "AND" = And
boolOpMap "OR" = Or
boolOpMap "NOR" = Nor
boolOpMap "NAND" = NAND
boolOpMap _ = undefined
cmpOpMap :: String -> CmpOp
cmpOpMap "<" = Less
cmpOpMap ">" = Greater
cmpOpMap "<=" = LessEq
cmpOpMap ">=" = GreaterEq
cmpOpMap "==" = Eq
cmpOpMap "!=" = NotEq
cmpOpMap "?=" = Eq  -- 特殊运算符，意为先判断存在再检查相等
cmpOpMap _ = undefined


expGen [ ''ValueUntypedNumExp, 
        ''ValueIntExp, 
        ''ValueFloatExp,
        ''ConstBoolExp,
        ''PossibleExp,  
        mkName "Switch", 
        ''Var, ''Color, 
        ''Text, 
        ''Identifier,
        mkName "Object",
        mkName "ObjectInList",
        ''Groups,
        ''BoolExp
         ]   -- 由于顺序问题这里两个涉及间接递归的类型似乎只能 mkName

-- 用模板实现了下面的代码
-- data Exp = FromValueUntypedNumExp ValueUntypedNumExp | 
--             FromValueIntExp ValueIntExp | 
--             FromValueFloatExp ValueFloatExp |
--             FromConstBoolExp ConstBoolExp | 
--             FromSwitch Switch | 
--             FromVar Var | 
--             FromColor Color | 
--             FromText Text |
--             FromObject Object
--             deriving (Show)
--instance Term ValueIntExp where
--    toExp = FromValueIntExp
--instance Term ValueFloatExp where
--    toExp = FromValueFloatExp
--instance Term ValueUntypedNumExp where
--    toExp = FromValueUntypedNumExp
--instance Term ConstBoolExp where
--    toExp :: ConstBoolExp -> Exp
--    toExp = FromConstBoolExp
--instance Term Switch where
--    toExp = FromSwitch
--instance Term Color where
--    toExp = FromColor
--instance Term Text where
--    toExp = FromText 
--instance Term Var where
--    toExp = FromVar
--instance Term Object where
--    toExp = FromObject

class ValueNum a where
    parseNum :: ParadoxParser a
    -- toValueExp :: a -> ValueExp a
    -- toValueExp = RawStaticalValue
    defaultValue :: a
instance ValueNum ValueInt where
    parseNum :: ParadoxParser ValueInt
    parseNum = parseInt
    defaultValue = 0 
instance ValueNum ValueFloat where
    parseNum :: ParadoxParser ValueFloat
    parseNum = parseFloat
    defaultValue = 0.0
instance ValueNum ValueUntypedNum where
    parseNum :: ParadoxParser ValueUntypedNum
    parseNum = parseUntypedNum
    defaultValue = ValueInt 0
data Switch = Switch {
    switch :: Key,
    cases :: [Exp]
} deriving (Show) 
data Declaration = Declaration {
    key :: Key,
    value :: Exp
} deriving (Show)
data Object = Object {
    obj_name :: Identifier,
    declarations :: DefinitionMap   -- 定义object时给出的属性
} deriving (Show)
data ObjectInList = ObjectInList {
    obj_name :: Text,
    declarations :: [(Key, Exp)]   -- 定义object时给出的属性
} deriving (Show)

type Effect = Object
type Effects = [Effect]
type ScopeMap = Map Identifier Text
type DefinitionMap = Map Identifier Exp
type Limit = BoolExp
liftToExp :: (Monad m, Term a) => m a -> m Exp 
liftToExp = liftM toExp

-- parser


-- lexer :: GenTokenParser Text s Identity
-- lexer = Tok.makeTokenParser paradoxLanguage
-- lexeme :: ParsecT Text s Identity a -> ParsecT Text s Identity a
-- lexeme = Tok.lexeme lexer
-- symbol :: String -> ParsecT Text s Identity String
-- symbol = Tok.symbol lexer
-- braces :: ParsecT Text s Identity a -> ParsecT Text s Identity a
-- braces = Tok.braces lexer

optionMaybe :: ParadoxParser a -> ParadoxParser (Maybe a)
optionMaybe = option Nothing . fmap Just
parseAnyTokens :: ParadoxParser Text
parseAnyTokens =  (lexeme.(fmap pack))
  ((:) <$> letterChar <*> many alphaNumChar <?> "tokens")
parseIntWithSpaceLeft :: ParadoxParser ValueInt
parseIntWithSpaceLeft = L.signed allSpace L.decimal
parseInt :: ParadoxParser ValueInt
parseInt = lexeme parseIntWithSpaceLeft
parseFloatWithSpaceLeft :: ParadoxParser ValueFloat
parseFloatWithSpaceLeft = try (L.signed allSpace L.float) <|> fmap fromIntegral parseIntWithSpaceLeft
parseFloat :: ParadoxParser ValueFloat
parseFloat = lexeme parseFloatWithSpaceLeft
parseUntypedNum :: ParadoxParser ValueUntypedNum
parseUntypedNum = try (fmap ValueFloat $ L.signed allSpace L.float) <|> fmap ValueInt parseInt
parseWhiteSpaces :: ParadoxParser ()
parseWhiteSpaces = allSpace
parseAnyReserved :: ParadoxParser Identifier
parseAnyReserved = lexeme $ do
    name <- foldl (<|>) empty (map  (fmap textToIdentifier . chunk) reservedNames)
    notFollowedBy alphaNumChar
    return name
parseIdentifier :: ParadoxParser Identifier
parseIdentifier = lexeme $ do
    name <- parseAnyTokens
    if name `elem` reservedNames then
        fail $ "unexpected reserved word " ++ show name
    else
        return $ textToIdentifier name
parseReserved :: Text -> ParadoxParser ()
parseReserved reserved_name 
    | reservedChecker reserved_name =
        lexeme $ do
        _ <- chunk reserved_name
        notFollowedBy alphaNumChar
        return ()
    | otherwise = undefined
        
parseReservedWithReturn :: Text -> ParadoxParser Text
parseReservedWithReturn str = do
    _ <- parseReserved str
    return str
parseReservedOp :: Text -> ParadoxParser ()
parseReservedOp str
    | reservedOpChecker str = chunk str >> notFollowedBy alphaNumChar
    | otherwise = undefined
-- parse Literal
parseText :: ParadoxParser Text
parseText = (lexeme . (fmap pack)) $ char '"' >> manyTill L.charLiteral (char '"')
parseColor :: ParadoxParser Color
parseColor = do
    colorType <- choice $ map parseReservedWithReturn ["hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360"]
    parseReservedOp "{"
    colorValue <- sepBy parseFloatWithSpaceLeft parseWhiteSpaces
    parseReservedOp "}"
    return $ Color colorType colorValue
parseVar :: ParadoxParser Var 
parseVar = do
    vartype <- option "" $ do 
        typeText <- parseIdentifier
        parseReservedOp ":"
        return $ identifierToText typeText
    name <- parseIdentifier
    return $ Var name vartype
identifierToVar :: Identifier -> Var
identifierToVar s = Var s (pack "")


    
    
