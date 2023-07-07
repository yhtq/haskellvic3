{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module UnTypedNumExpParser(
    parseValueIntExp,
    parseValueFloatExp,
    parseValueUntypedNumExp,
    ParadoxUntypedNumParser,
    valueUntypedNumFloatSimplify,
    valueUntypedNumIntSimplify,
) where
import BaseParser
import qualified Data.Text as DT
import Text.Parsec ( chainl, option, (<|>), getState, optionMaybe, try, modifyState, setState, char )
import Data.Text (pack, Text)
import Control.Monad (when)
type ParadoxUntypedNumParser = StatedParadoxParser NumTypeFlag
class StateFlag s where
    isDefault :: s -> Bool
    setInt :: s -> s
    setFloat :: s -> s
    -- 合并两个flag（找公共类型）
    merge :: s -> s -> s
class (ValueNum a, StateFlag s) => DynamicParser a s | a -> s where
    getParser :: s -> StatedParadoxParser s a
instance StateFlag () where
    isDefault :: () -> Bool
    isDefault _ = False
    setInt :: () -> ()
    setInt _ = ()
    setFloat :: () -> ()
    setFloat _ = ()
    merge :: () -> () -> ()
    merge _ _ = ()
instance StateFlag NumTypeFlag where
    isDefault :: NumTypeFlag -> Bool
    isDefault = (==) Int
    setInt :: NumTypeFlag -> NumTypeFlag
    setInt _ = Int
    setFloat :: NumTypeFlag -> NumTypeFlag
    setFloat _ = Float
    merge :: NumTypeFlag -> NumTypeFlag -> NumTypeFlag
    merge Int Int = Int
    merge _ _ = Float
instance DynamicParser ValueInt NumTypeFlag where
    getParser :: NumTypeFlag -> StatedParadoxParser NumTypeFlag ValueInt
    getParser _ = parseInt
instance DynamicParser ValueFloat NumTypeFlag where
    getParser :: NumTypeFlag -> StatedParadoxParser NumTypeFlag ValueFloat
    getParser _ = parseFloat
instance DynamicParser ValueUntypedNum NumTypeFlag where
    getParser :: NumTypeFlag -> StatedParadoxParser NumTypeFlag ValueUntypedNum
    getParser Int = fmap ValueInt parseInt'
    getParser Float = fmap ValueFloat parseFloat
-- parseUntypedNumExp :: ParadoxParser ValueUntypedNumExp
-- parseUntypedNumExp = parseValueNumExp :: ParadoxParser ValueUntypedNumExp
type UntypedNumParser = StatedParadoxParser NumTypeFlag
parseInt' = try $ do
    int <- parseInt
    pointTry <- optionMaybe (char '.')
    case pointTry of
        Just _ -> fail "not int"
        Nothing -> return int
parseNotLiteralNum :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseNotLiteralNum = fmap (RawVar . Var) parseIdentifier <|> fmap RawScriptedValue parseText
parseValueNumExp :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumRaw = do
    valType <- getState
    result <- optionMaybe (try $ getParser valType)
    case result of
        Just value -> return (RawStaticalValue value)
        Nothing ->
            if isDefault valType
            then do
                result2 <- optionMaybe parseNotLiteralNum
                case result2 of
                    Just value -> return value
                    Nothing -> do
                        -- 这里不需要再Maybe了，若失败理应直接报错
                        let floatParser = getParser (setFloat valType)
                        resultfloat <- floatParser
                        modifyState setFloat
                        return (RawStaticalValue resultfloat)
            else parseNotLiteralNum
parseValueNumExpBlock :: (ValueNum a, StateFlag s, DynamicParser a s) => StatedParadoxParser s (ValueExp a)
parseValueNumExpBlock = do
    parseReserved "{"
    exp <- parseValueNumExp'
    parseReserved "}"
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
        ValueFloat f -> undefined   -- 这里不应该出现Float型
    )
parseValueIntExp :: ParadoxParser (ValueExp ValueInt)
parseValueIntExp = parseValueNumExp
parseValueFloatExp :: ParadoxParser (ValueExp ValueFloat)
parseValueFloatExp = parseValueNumExp
parseValueUntypedNumExp :: ParadoxParser (ValueExp ValueUntypedNum)
parseValueUntypedNumExp = parseValueNumExp
