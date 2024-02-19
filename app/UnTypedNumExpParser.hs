{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module UnTypedNumExpParser(
    parseValueIntExp,
    parseValueFloatExp,
    parseValueUntypedNumExp,
    ParadoxUntypedNumParser,
    valueUntypedNumFloatSimplify,
    valueUntypedNumIntSimplify,
) where
import Prelude hiding (exp)
import BaseParser
import qualified Data.Text as DT
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Text (pack, Text)
import Control.Monad.Trans.Class(lift)
import Control.Monad.State.Class(get, put)
import Control.Monad.State.Lazy(StateT, runStateT)


-- 这里的 flag 用于标记当前的解析器是解析Int还是Float，规则为在同一个表达式中，所有字面值都是整数则为Int，否则为Float
type ParadoxUntypedNumParser = StatedParadoxParser NumTypeFlag
newtype IntOnlyFlag = IntOnlyFlag ()
newtype FloatOnlyFlag = FloatOnlyFlag ()
class StateFlag s where
    isInt :: s -> Bool
    isFloat :: s -> Bool
    isFloat = not . isInt
    setInt :: s -> s
    setFloat :: s -> s
    -- 合并两个flag（找公共类型）
    merge :: s -> s -> s
class (ValueNum a, StateFlag s) => DynamicParser a s | a -> s where
    getParser :: s -> ParadoxParser a
instance StateFlag NumTypeFlag where
    -- 判断类型标志是否为默认值
    isInt :: NumTypeFlag -> Bool
    isInt = (==) Int
    setInt :: NumTypeFlag -> NumTypeFlag
    setInt _ = Int
    setFloat :: NumTypeFlag -> NumTypeFlag
    setFloat _ = Float
    merge :: NumTypeFlag -> NumTypeFlag -> NumTypeFlag
    merge Int Int = Int
    merge _ _ = Float
instance StateFlag IntOnlyFlag where
    -- 只解析整数，在解析到浮点数时报错
    isInt :: IntOnlyFlag -> Bool
    isInt _ = True
    setInt :: IntOnlyFlag -> IntOnlyFlag
    setInt _ = IntOnlyFlag ()
    setFloat :: IntOnlyFlag -> IntOnlyFlag
    setFloat _ = error "catch float in IntParser"
    merge :: IntOnlyFlag -> IntOnlyFlag -> IntOnlyFlag
    merge _ _ = IntOnlyFlag ()
instance StateFlag FloatOnlyFlag where
    -- 只解析浮点数，在解析到整数时报错
    isInt :: FloatOnlyFlag -> Bool
    isInt _ = False
    setInt :: FloatOnlyFlag -> FloatOnlyFlag
    setInt _ = FloatOnlyFlag ()
    setFloat :: FloatOnlyFlag -> FloatOnlyFlag
    setFloat _ = FloatOnlyFlag ()
    merge :: FloatOnlyFlag -> FloatOnlyFlag -> FloatOnlyFlag
    merge _ _ = FloatOnlyFlag ()

instance DynamicParser ValueUntypedNum NumTypeFlag where
    getParser :: NumTypeFlag -> ParadoxParser ValueUntypedNum
    getParser Int =  fmap ValueInt parseInt
    getParser Float = fmap ValueFloat (parseFloat)
instance DynamicParser ValueInt IntOnlyFlag where
    getParser :: IntOnlyFlag -> ParadoxParser ValueInt
    getParser _ =  parseInt' :: ParadoxParser ValueInt
instance DynamicParser ValueFloat FloatOnlyFlag where
    getParser :: FloatOnlyFlag -> ParadoxParser ValueFloat
    getParser _ =  parseFloat :: ParadoxParser ValueFloat
-- parseUntypedNumExp :: ParadoxParser ValueUntypedNumExp
-- parseUntypedNumExp = parseValueNumExp :: ParadoxParser ValueUntypedNumExp
parseInt' :: ParadoxParser ValueInt
parseInt' = try $ do
    int <- parseInt
    notFollowedBy (char '.')
    return int
parseNotLiteralNum :: (ValueNum a) => ParadoxParser (ValueExp a)
parseNotLiteralNum = fmap RawIdentifier parseIdentifier <|> fmap RawScriptedValue parseText
parseValueNumExp :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumRaw = do
    valType <- get
    let parser = (getParser :: s -> ParadoxParser a) valType
    result <-  lift $ optionMaybe (try parser)
    case result of
        Just value -> return (RawStaticalValue value)
        Nothing ->
            if isInt valType
            then do
                result2 <- lift $ optionMaybe parseNotLiteralNum
                case result2 of
                    Just value -> return value
                    Nothing -> do
                        -- 这里不需要再Maybe了，若失败理应直接报错
                        let floatParser = (getParser :: s -> ParadoxParser a) (setFloat valType)
                        resultfloat <- lift floatParser
                        put (setFloat valType)
                        return (RawStaticalValue resultfloat)
            else lift parseNotLiteralNum
parseValueNumExpBlock :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumExpBlock = do
    lift $ parseReserved "{"
    exp <- parseValueNumExp'
    lift $ parseReserved "}"
    return exp
parseAppendValueNumExp :: (ValueNum a, StateFlag s, DynamicParser a s)
                            => (StatedParadoxParser s (Maybe (AppendingValueExp a), Text))
parseAppendValueNumExp = do
                            let append' op = do
                                        parseReservedOp "="
                                        exp <- parseValueNumExp
                                        return (Just $ ExpAppending op exp, pack "")
                            let append = (do {parseReservedOp "add"; append' Add})
                                    <|> (do {parseReservedOp "multiply"; append' Multiply})
                                    <|> (do {parseReservedOp "subtract"; append' Subtract})
                                    <|> (do {parseReservedOp "divide"; append' Divide})
                                    <|> (do {parseReservedOp "min"; append' Min})
                                    <|> (do {parseReservedOp "max"; append' Max})
                                    <|> (do {parseReservedOp "modulo"; append' Modulo})
                                    <|> (do {parseReservedOp "round"; append' Round})
                                    <|> (do {parseReservedOp "ceiling"; append' Ceiling})
                                    <|> (do {parseReservedOp "floor"; append' Floor})
                                    <|> (do {parseReservedOp "round_to"; append' RoundTo})
                                    <|> (do
                                            parseReserved "desc"
                                            parseReservedOp "="
                                            desc <- parseText
                                            return (Nothing, desc)
                                        )
                            let concatAppending a1 a2 = case (a1, a2) of
                                    (Just (ExpAppending op1 exp1), Nothing) -> Just (ExpAppending op1 exp1)
                                    (Nothing, Just (ExpAppending op2 exp2)) -> Just (ExpAppending op2 exp2)
                                    (Nothing, Nothing) -> Nothing
                                    (Just ea1, Just (ExpAppending op2 exp2)) -> Just (ExpAppendings ea1 op2 exp2)
                                    _ -> undefined -- 理应不出现
                            let concatWithDesc (a1, desc1) (a2, desc2) = (concatAppending a1 a2, desc1 <> desc2)
                            (f, desc) <- chainl append (return concatWithDesc) (Nothing, pack "")
                            return (f, desc)


parseValueNumExp' :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumExp' = do
                        base <- option (RawStaticalValue defaultValue ) (
                                do
                                    parseReserved "value"
                                    parseReservedOp "="
                                    parseValueNumExp
                                )
                        (app, desc) <- parseAppendValueNumExp
                        case app of
                            Just appending -> (if DT.null desc then return (Exp base appending) else return (ValueExpWithDesc desc (Exp base appending)))
                            Nothing -> if DT.null desc then return (ValueExpWithDesc desc base) else fail "In parseValueNumExp"
-- 将所有Untyped型转为Float型
valueUntypedNumFloatSimplify :: ValueExp ValueUntypedNum -> ValueExp ValueFloat
valueUntypedNumFloatSimplify = valueMap (\case
        ValueInt i -> fromIntegral i
        ValueFloat f -> f
    )
-- 将所有Untyped型转为Int型
valueUntypedNumIntSimplify :: ValueExp ValueUntypedNum -> ValueExp ValueInt
valueUntypedNumIntSimplify = valueMap (\case
        ValueInt i -> i
        ValueFloat _ -> undefined   -- 这里不应该出现Float型
    )
parseValueIntExp :: ParadoxParser (ValueExp ValueInt)
parseValueIntExp = do
    (res, _) <- runStateT parseValueNumExp (IntOnlyFlag ())
    return res
parseValueFloatExp :: ParadoxParser (ValueExp ValueFloat)
parseValueFloatExp = do
    (res, _) <- runStateT parseValueNumExp (FloatOnlyFlag ())
    return res
parseValueUntypedNumExp :: ParadoxParser (ValueExp ValueUntypedNum)
parseValueUntypedNumExp = do
    (res, _) <- runStateT parseValueNumExp (Int)
    return res
