{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser
(
    parseObjects,
    runTestParser
) where
import Prelude hiding (exp, lookup)
import BaseParser
import UnTypedNumExpParser ( parseValueUntypedNumExp, valueUntypedNumFloatSimplify, valueUntypedNumIntSimplify )
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Map (Map, lookup, fromListWithKey, fromList)
import BoolParser (parseConstBoolExp, parseBoolExps, parsePossibleExp)
import Data.Maybe (fromMaybe)
parseObject' :: Maybe Text -> ParadoxParser Object
parseObject' optionalName = do
    parseWhiteSpaces
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
    let errorReport k _ _ = fail $ "duplicate key: " ++ DT.unpack k
    let declarationsMapParse = sequence $ buildMap errorReport declarations
    declarationsMap <- declarationsMapParse
    return $ Object name declarationsMap
parseObject :: ParadoxParser Object
parseObject = parseObject' Nothing
parseObjects :: ParadoxParser [Object]
parseObjects = do
    parseWhiteSpaces
    many1 parseObject

-- 若有特殊关键字则直接制导到对应的Parser
type KeyParserMap = Map Text (ParadoxParser Exp)
defaultParser :: ParadoxParser Exp
defaultParser = parseExp
defaultKeyParserMap :: KeyParserMap
defaultKeyParserMap = fromList [
        (DT.pack "possible", liftToExp parsePossibleExp)
    ]

getParser :: Text -> KeyParserMap -> ParadoxParser Exp
getParser key parserMap = fromMaybe defaultParser (lookup key parserMap)

parseDeclaration :: KeyParserMap -> ParadoxParser Declaration
parseDeclaration parserMap = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- getParser key parserMap <|> liftToExp (parseObject' $ Just key)
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
    setState Int
    exp <- try parseValueUntypedNumExp
    state <- getState
    let newValueExp = if state == Float then FromValueFloatExp $ valueUntypedNumFloatSimplify exp else FromValueIntExp $ valueUntypedNumIntSimplify exp
    return newValueExp
parseGroups :: ParadoxParser Groups
parseGroups = try $ do
    parseReserved "{"
    groups <- many1 parseIdentifier
    parseReserved "}"
    return groups
parseExp :: ParadoxParser Exp
parseExp = do
    liftToExp parseGroups
    <|> parseExpBlock
    <|> liftToExp parseIdentifier
    <|> liftToExp parseText
    -- 由于重叠问题（ValueExp也可以由字面文本或者关键字构成），这里先尝试直接解析留到后面处理
    <|> parseValueExp
    -- <|> liftToExp parseVar 
    <|> liftToExp parseColor
    <|> liftToExp parseConstBoolExp
    -- parseBoolExp <|> parseColorExp 
parseExpBlock :: ParadoxParser Exp
parseExpBlock = Tok.braces lexer parseExp
runTestParser :: ParadoxParser a -> Text -> Either ParseError a
runTestParser p = runParser p Int ""