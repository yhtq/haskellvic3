{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
(
    parseObjects,
    runTestParser,
    parseExp,
    parseDeclaration,
    defaultKeyParserMap
) where
import Prelude hiding (exp, lookup)
import BaseParser
import UnTypedNumExpParser ( parseValueUntypedNumExp, valueUntypedNumFloatSimplify, valueUntypedNumIntSimplify )
import Text.Megaparsec
import Data.Text ()
import Data.Map (Map, lookup, fromListWithKey, fromList)
import BoolParser (parseConstBoolExp)
import Data.Maybe (fromMaybe)
import Control.Monad.State (runStateT)
parseObject' :: Maybe Identifier -> ParadoxParser Object
parseObject' optionalName = lexeme $ do
    name <- case optionalName of
        Nothing -> do
            name <- parseIdentifier
            parseReservedOp "="
            return name
        Just name -> return name 
    parseReserved "{"
    declarations <- many $ parseDeclaration defaultKeyParserMap
    parseReserved "}"
    let buildMap f = fromListWithKey f.map (\(Declaration k v) -> (k, return v :: ParadoxParser Exp))
    let errorReport k _ _ = fail $ "duplicate key: " ++ identifierToString k
    let declarationsMapParse = sequence $ buildMap errorReport declarations
    declarationsMap <- declarationsMapParse
    return $ Object name declarationsMap
parseObject :: ParadoxParser Object
parseObject = parseObject' Nothing
parseObjects :: ParadoxParser [Object]
parseObjects = lexeme $ do
    parseWhiteSpaces
    some parseObject

-- 若有特殊关键字则直接制导到对应的Parser
type KeyParserMap = Map Key (Identifier -> ParadoxParser Exp)
defaultParser :: Identifier -> ParadoxParser Exp
defaultParser = parseExp
defaultKeyParserMap :: KeyParserMap
defaultKeyParserMap = fromList [
    ]

getParser :: Key -> KeyParserMap -> ParadoxParser Exp
getParser key parserMap = fromMaybe (defaultParser) (lookup key parserMap) key

parseDeclaration :: KeyParserMap -> ParadoxParser Declaration
parseDeclaration parserMap = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- (getParser key parserMap) <|> liftToExp (parseObject' $ Just key)
    return $ Declaration key value
--parseValueExpWithLabel :: ParadoxUntypedNumParser Exp
--parseValueExpWithLabel = do
--    liftToExp $ 
    -- liftToExp $ try parseValueIntExp 
    -- <|>
    -- liftToExp parseValueFloatExp 
    -- <|> parseBoolExp <|> parseVarExp <|> parseColorExp <|> parseText
parseValueExp :: ParadoxParser Exp
parseValueExp = do
    (exp, flag) <- parseValueUntypedNumExp
    let newValueExp = if flag == Float then FromValueFloatExp $ valueUntypedNumFloatSimplify exp else FromValueIntExp $ valueUntypedNumIntSimplify exp
    return newValueExp
parseGroups :: ParadoxParser Groups
parseGroups =  do
    parseReserved "{"
    groups <- some parseIdentifier
    parseReserved "}"
    return groups
parseExp :: Identifier -> ParadoxParser Exp
parseExp key = foldl1 (<|>) $ map try [
    liftToExp parseGroups,
    liftToExp parseIdentifier,
    liftToExp parseText,
    -- 由于重叠问题（ValueExp也可以由字面文本或者关键字构成），这里先尝试直接解析留到后面处理
    parseValueExp,
    -- , liftToExp parseVar 
    liftToExp parseColor,
    liftToExp parseConstBoolExp,
    liftToExp (parseObject' $ Just key)]
    -- parseBoolExp <|> parseColorExp 
-- parseExpBlock :: ParadoxParser Exp
-- parseExpBlock = braces parseExp
runTestParser :: GlobalState -> ParadoxParser a -> TokenType -> (Either (ParseErrorBundle TokenType ErrorType) a)
runTestParser s p t = do
    (res, _) <- runParser (runStateT p s) "" t 
    return res