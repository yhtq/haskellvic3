{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where
import BaseParser
import TypedNumExpParser
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Map (fromListWithKey)
parseObject :: ParadoxParser Object
parseObject = do
    parseWhiteSpaces
    name <- parseIdentifier
    parseReservedOp "="
    parseReserved "{"
    assignments <- many parseAssignment
    parseReserved "}"
    let buildMap f = fromListWithKey f.map (\(Assignment k v) -> (k, return v :: ParadoxParser Exp))
    let errorReport k _ _ = fail $ "duplicate key: " ++ DT.unpack k
    let assignmentsMapParse = sequence $ buildMap errorReport assignments
    assignmentsMap <- assignmentsMapParse
    return $ Object name assignmentsMap
parseObjects :: ParadoxParser [Object]
parseObjects = do
    parseWhiteSpaces
    many1 parseObject
parseAssignment :: ParadoxParser Assignment
parseAssignment = do
    key <- parseIdentifier
    parseReservedOp "="
    value <- parseExp
    return $ Assignment key value

parseExp :: ParadoxParser Exp
parseExp = do
    parseExpBlock 
    <|> parseValueExp 
    -- 暂且统一视作Value?
    -- <|> liftToExp parseVar 
    <|> liftToExp parseText
    <|> liftToExp parseColor
    <|> liftToExp parseIdentifier
    -- parseBoolExp <|> parseColorExp 
parseExpBlock :: ParadoxParser Exp
parseExpBlock = Tok.braces lexer parseExp
runTestParser :: ParadoxParser a -> Text -> Either ParseError a
runTestParser p = parse p ""