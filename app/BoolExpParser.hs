{-# LANGUAGE AllowAmbiguousTypes #-}
module BoolExpParser where
import BaseParser
import UnTypedNumExpParser (parseValueUntypedNumExp, parseValueIntExp, parseValueFloatExp)
import Text.Parsec (choice, many1, (<|>), try, optionMaybe)
import Data.Text (unpack, pack)
parseBoolExp :: ParadoxParser BoolExp
parseBoolExp = do
    parseBoolOp <|> parseBoolOp' <|> parseCmp @ValueUntypedNum <|> parseQ <|> parseScopeTrans 
parseBoolOp :: ParadoxParser BoolExp
parseBoolOp = do
    op <- choice $ map parseReservedWithReturn ["AND", "OR", "NOR", "NAND"]
    let opConstructor = boolOpMap $ unpack op
    exps <- braces $ many1 parseBoolExp
    return (BoolOp opConstructor exps) 
parseBoolOp' :: ParadoxParser BoolExp
parseBoolOp' = do
    parseReserved "NOT"
    exp <- braces parseBoolExp
    return (BoolOp' Not exp)
class (ValueNum a) => Cmp a where
    parseExp :: ParadoxParser (ValueExp a)
    toBoolExp :: CmpOp -> (ValueExp a) -> (ValueExp a) -> BoolExp
instance Cmp ValueInt where
    parseExp = parseValueIntExp
    toBoolExp = IntCmp
instance Cmp ValueFloat where
    parseExp = parseValueFloatExp
    toBoolExp = FloatCmp
instance Cmp ValueUntypedNum where
    parseExp :: ParadoxParser (ValueExp ValueUntypedNum)
    parseExp = parseValueUntypedNumExp
    toBoolExp = UntypedNumCmp
parseCmp :: forall a.(ValueNum a, Cmp a) => ParadoxParser BoolExp
parseCmp = try $ do
    exp1 <- parseExp @a
    op <- choice $ map parseReservedWithReturn ["<=", ">=", "<", ">", "?=", "!=", "=="]
    exp2 <- parseExp @a
    let opConstructor = cmpOpMap $ unpack op
    return (toBoolExp opConstructor exp1 exp2)
parseQ :: ParadoxParser BoolExp
parseQ = try $ do
    query <- parseIdentifier
    parseReservedOp "=" <|> parseReservedOp "?="
    key <- parseIdentifier <|> choice (map  parseReservedWithReturn ["yes", "no"])
    return (Q query key)
parseScopeTrans :: ParadoxParser BoolExp
parseScopeTrans = try $ do 
    scope <- optionMaybe $ 
        try $ do 
            key <- parseIdentifier
            parseReservedOp ":"
            return key
    let scope' = case scope of
            Just s -> s
            Nothing -> pack ""
    trans <- parseIdentifier
    parseReservedOp "=" <|> parseReservedOp "?="
    exp <- braces parseBoolExp
    return (ScopeTrans (ScopeTransformer scope' trans) exp)
parseBoolRaw :: ParadoxParser ConstBoolExp
parseBoolRaw = do
    rawValue <- choice $ map parseReservedWithReturn ["yes", "no"]
    if rawValue == pack "yes" then return $ BoolRaw Yes else return $ BoolRaw No
    


