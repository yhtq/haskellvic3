{-# LANGUAGE AllowAmbiguousTypes #-}
module BoolParser(
    parseBoolExps,
    parseConstBoolExp,
    parsePossibleExp
) where
import Prelude hiding (exp)
import BaseParser
import UnTypedNumExpParser (parseValueUntypedNumExp, parseValueIntExp, parseValueFloatExp)
import Text.Parsec (choice, many1, (<|>), try, optionMaybe)
import Data.Text (unpack, pack)
import Control.Monad ((>=>))
-- 由于实际的BoolExp提供了直接并列的语法糖故这里需要稍微写一下
parseBoolExps :: ParadoxParser BoolExp
parseBoolExps = do
    exps <- many1 parseBoolExp
    return (BoolOp And exps)
parseBoolExp :: ParadoxParser BoolExp
parseBoolExp = do
    parseBoolOp <|> parseBoolOp' <|> parseCmp @ValueUntypedNum <|> parseQ <|> parseScopeTrans
parseConstBoolExp :: ParadoxParser ConstBoolExp
parseConstBoolExp = parseBoolRaw
parseBoolOp :: ParadoxParser BoolExp
parseBoolOp = do
    op <- choice $ map parseReservedWithReturn ["AND", "OR", "NOR", "NAND"]
    let opConstructor = boolOpMap $ unpack op
    parseReservedOp "="
    exps <- braces $ many1 parseBoolExp
    return (BoolOp opConstructor exps)
parseBoolOp' :: ParadoxParser BoolExp
parseBoolOp' = do
    parseReserved "NOT"
    parseReservedOp "="
    exp <- braces parseBoolExp
    return (BoolOp' Not exp)
class (ValueNum a) => Cmp a where
    parseExp :: ParadoxParser (ValueExp a)
    toBoolExp :: CmpOp -> (ValueExp a) -> (ValueExp a) -> BoolExp
instance Cmp ValueInt where
    parseExp :: ParadoxParser (ValueExp ValueInt)
    parseExp = parseValueIntExp
    toBoolExp :: CmpOp -> ValueExp ValueInt -> ValueExp ValueInt -> BoolExp
    toBoolExp = IntCmp
instance Cmp ValueFloat where
    parseExp :: ParadoxParser (ValueExp ValueFloat)
    toBoolExp :: CmpOp -> ValueExp ValueFloat -> ValueExp ValueFloat -> BoolExp
    parseExp = parseValueFloatExp
    toBoolExp = FloatCmp
instance Cmp ValueUntypedNum where
    parseExp :: ParadoxParser (ValueExp ValueUntypedNum)
    parseExp = parseValueUntypedNumExp
    toBoolExp :: CmpOp -> ValueExp ValueUntypedNum -> ValueExp ValueUntypedNum -> BoolExp
    toBoolExp = UntypedNumCmp

-- 注意下面的函数多态是有歧义的，必须指定类型才能使用
parseCmp :: forall a.(ValueNum a, Cmp a) => ParadoxParser BoolExp
parseCmp = try $ do
    exp1 <- parseExp @a
    op <- choice $ map parseReservedWithReturn ["<=", ">=", "<", ">", "?=", "!=", "=="]
    exp2 <- parseExp @a
    let opConstructor = cmpOpMap $ unpack op
    return (toBoolExp opConstructor exp1 exp2)
parseQ :: ParadoxParser BoolExp
parseQ = try $ do
    query <- parseVar
    parseReservedOp "=" <|> parseReservedOp "?="
    key <- parseVar <|> choice (map  (parseReservedWithReturn >=> (return . identifierToVar . textToIdentifier))
         ["yes", "no"])
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
            Nothing -> stringToIdentifier ""
    trans <- parseIdentifier
    parseReservedOp "=" <|> parseReservedOp "?="
    exp <- braces parseBoolExp
    return (ScopeTrans (ScopeTransformer scope' trans) exp)
parseBoolRaw :: ParadoxParser ConstBoolExp
parseBoolRaw = do
    rawValue <- choice $ map parseReservedWithReturn ["yes", "no"]
    if rawValue == pack "yes" then return $ BoolRaw Yes else return $ BoolRaw No
parseErrorCheck :: ParadoxParser ErrorCheck
parseErrorCheck = do
    parseReserved "error_check"
    parseReservedOp "="
    (sever, exp) <- braces $ do
        parseReserved "severity"
        parseReservedOp "="
        sever <- parseIdentifier
        exp <- parseBoolExp
        return (sever, exp)
    return $ ErrorCheck sever exp
parsePossibleExp :: ParadoxParser PossibleExp
parsePossibleExp = do
    braces $ fmap WithErrorCheck (many1 parseErrorCheck) <|> fmap Possible parseBoolExp



