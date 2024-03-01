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
import Numeric
import Text.Megaparsec 
import Data.Text (Text, pack, unpack, append)
import Data.Map (Map) 
import Data.Set (singleton)
import Prettyprinter(Pretty(..))
import Data.String (IsString(..))
import Data.List.NonEmpty (fromList)
import Control.Monad(liftM)
import Data.Void(Void)
import Control.Monad.Trans.State.Lazy (StateT)
import TemplateExp ( expGen )
-- import Text.MegaParsec.Token (GenTokenParser(stringLiteral))
import Language.Haskell.TH (mkName)
data NumTypeFlag = Int | Float deriving (Show, Eq)
type TokenType = Text
type ErrorType = Void
type GlobalState = ()
-- type ParadoxLanguage s = Tok.GenLanguageDef Text s Identity
type ParadoxParser  = StateT GlobalState (Parsec ErrorType TokenType)
type StatedParadoxParser s  = StateT s (ParadoxParser)

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
     --    "error_check", "severity",
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
comment :: (MonadParsec e TokenType m) => m ()
comment = L.skipLineComment ("#")
commentBlock :: (MonadParsec e TokenType m) => m ()
commentBlock = L.skipBlockCommentNested ("/*") ("*/")
spaceConsumer :: (MonadParsec e TokenType m) => m ()
spaceConsumer = C.space1
allSpace :: (MonadParsec e TokenType m) => m ()
allSpace = L.space spaceConsumer comment commentBlock
lexeme :: (MonadParsec e TokenType m) => m a -> m a
lexeme = L.lexeme allSpace

-- 类型定义
type Undetermined = ()
newtype Identifier = Text Text deriving (Show, Eq, Ord)
type Key = Identifier
instance IsString Identifier where
    fromString = Text . pack
instance Pretty Identifier where
    pretty (Text s) = pretty s
initState :: GlobalState
initState = ()
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

-- 打印浮点数时保留的小数位数
floatPrintingNumber :: Int
floatPrintingNumber = 3

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
type Dec = (Key, Exp)
instance Term ValueBool where
    toExp :: ValueBool -> Exp
    toExp = FromConstBoolExp . BoolRaw
instance Term ValueInt where
    toExp :: ValueInt -> Exp
    toExp = FromValueIntExp . RawStaticalValue
instance Term ValueFloat where
    toExp :: ValueFloat -> Exp
    toExp = FromValueFloatExp . RawStaticalValue

class  ValueNum a where
    parseNum :: (MonadParsec e TokenType m) => m a
    -- toValueExp :: a -> ValueExp a
    -- toValueExp = RawStaticalValue
    defaultValue :: a
    printNum :: a -> Text
instance ValueNum ValueInt where
    parseNum :: (MonadParsec e TokenType m) => m ValueInt
    parseNum = parseInt
    defaultValue = 0 
    printNum :: ValueInt -> Text
    printNum = pack . show
instance ValueNum ValueFloat where
    parseNum :: (MonadParsec e TokenType m) => m ValueFloat
    parseNum = parseFloat
    defaultValue = 0.0
    printNum :: ValueFloat -> Text
    printNum x = pack $ showFFloat (Just floatPrintingNumber) x ""
instance ValueNum ValueUntypedNum where
    parseNum :: (MonadParsec e TokenType m) => m ValueUntypedNum
    parseNum = parseUntypedNum
    defaultValue = ValueInt 0
    printNum :: ValueUntypedNum -> Text
    printNum (ValueInt x) = pack $ show x
    printNum (ValueFloat x) = pack $ showFFloat (Just floatPrintingNumber) x ""
data Switch = Switch {
    switch :: Key,
    cases :: [Exp]
} deriving (Show) 
type Declaration = Dec
data Object = Object {
    obj_name :: Identifier,
    declarations :: DefinitionMap   -- 定义object时给出的属性
} deriving (Show)
data ObjectInList = ObjectInList {
    obj_name :: Text,
    declarations :: [Dec]   -- 定义object时给出的属性
} deriving (Show)

type Effect = Object
type Effects = [Effect]
type ScopeMap = Map Identifier Text
type DefinitionMap = Map Identifier Exp
type Limit = BoolExp
liftToExp :: (Monad m, Term a) => m a -> m Exp 
liftToExp = liftM toExp
textToErrorItem :: Text -> ErrorItem (Token Text)
textToErrorItem = Tokens . fromList . unpack
-- parser


-- lexer :: GenTokenParser Text s Identity
-- lexer = Tok.makeTokenParser paradoxLanguage
-- lexeme :: ParsecT Text s Identity a -> ParsecT Text s Identity a
-- lexeme = Tok.lexeme lexer
-- symbol :: String -> ParsecT Text s Identity String
-- symbol = Tok.symbol lexer
-- braces :: ParsecT Text s Identity a -> ParsecT Text s Identity a
-- braces = Tok.braces lexer

optionMaybe :: (MonadParsec e TokenType m) => m a -> m (Maybe a)
optionMaybe p = option Nothing $ try (
    do 
        res <- p
        return $ Just res
    )
parseAnyTokens :: (MonadParsec e TokenType m) => m Text
parseAnyTokens =  (lexeme.(fmap pack))
  ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "tokens")
parseIntWithSpaceLeft :: (MonadParsec e TokenType m) => m ValueInt
parseIntWithSpaceLeft = do
    res <- L.signed allSpace L.decimal
    notFollowedBy (char '.')
    return res
parseInt :: (MonadParsec e TokenType m) => m ValueInt
parseInt = lexeme parseIntWithSpaceLeft
parseFloatWithSpaceLeft :: (MonadParsec e TokenType m) => m ValueFloat
parseFloatWithSpaceLeft = try (L.signed allSpace L.float) <|> fmap fromIntegral parseIntWithSpaceLeft
parseFloat :: (MonadParsec e TokenType m) => m ValueFloat
parseFloat = lexeme parseFloatWithSpaceLeft
parseUntypedNum :: (MonadParsec e TokenType m) => m ValueUntypedNum
parseUntypedNum = try (fmap ValueFloat $ L.signed allSpace L.float) <|> fmap ValueInt parseInt
parseWhiteSpaces :: (MonadParsec e TokenType m) => m ()
parseWhiteSpaces = allSpace
parseAnyReserved :: (MonadParsec e TokenType m) => m Identifier
parseAnyReserved = lexeme $ do
    name <- foldl (<|>) empty (map  (fmap textToIdentifier . chunk) reservedNames)
    notFollowedBy alphaNumChar
    return name
parseIdentifier :: (MonadParsec e TokenType m) => m Identifier
parseIdentifier = lexeme $ try $ do
    name <- parseAnyTokens
    if name `elem` reservedNames then
        failure (Just $ textToErrorItem name) (singleton $ textToErrorItem $ ("unexpected reserved word " `append` name))
    else
        return $ textToIdentifier name
parseReserved :: (MonadParsec e TokenType m) => Text -> m ()
parseReserved reserved_name 
    | reservedChecker reserved_name =
        lexeme $ do
        _ <- chunk reserved_name
        notFollowedBy alphaNumChar
        return ()
    | otherwise = failure (Just $ textToErrorItem reserved_name) (singleton $ textToErrorItem $ ("unexpected reserved word " `append` reserved_name))
        
parseReservedWithReturn :: (MonadParsec e TokenType m) => Text -> m Text
parseReservedWithReturn str = do
    _ <- parseReserved str
    return str
parseReservedOp :: (MonadParsec e TokenType m) => Text -> m ()
parseReservedOp str
    | reservedOpChecker str = lexeme $ chunk str >> notFollowedBy alphaNumChar
    | otherwise = failure (Just $ textToErrorItem str) (singleton $ textToErrorItem $ ("unexpected reserved operator " `append` str))
-- parse Literal
parseText :: (MonadParsec e TokenType m) => m Text
parseText = (lexeme . (fmap pack)) $ char '"' >> manyTill L.charLiteral (char '"')
parseColor :: (MonadParsec e TokenType m) => m Color
parseColor = lexeme $ do
    colorType <- choice $ map parseReservedWithReturn ["hsv", "HSV", "rgb", "RGB", "hsv360", "HSV360"]
    parseReservedOp "{"
    colorValue <- sepBy parseFloatWithSpaceLeft parseWhiteSpaces
    parseReservedOp "}"
    return $ Color colorType colorValue
parseVar :: (MonadParsec e TokenType m) => m Var 
parseVar = lexeme $ do
    vartype <- option "" $ do 
        typeText <- parseIdentifier
        parseReservedOp ":"
        return $ identifierToText typeText
    name <- parseIdentifier
    return $ Var name vartype
braces :: (MonadParsec e TokenType m) => m a -> m a
braces = between (parseReservedOp "{") (parseReservedOp "}")
identifierToVar :: Identifier -> Var
identifierToVar s = Var s (pack "")

chainl :: (MonadParsec e TokenType m) => m a -> m (a -> a -> a) -> a -> m a
chainl p op a = (foldl (\x f -> f x) a) <$> (many (flip <$> op <*> p))
    
    
