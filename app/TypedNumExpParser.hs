module TypedNumExpParser where
import BaseParser
import qualified Data.Text as DT
import Data.Map (Map) 
import qualified Text.Parsec.Token as Tok
import Data.Text (Text, pack)
import Text.Parsec 
-- 或许可以优化，解析整数失败不一定要回溯
parseValueExp :: ParadoxParser Exp
parseValueExp = do
    liftToExp $ try parseUntypedNumExp
    -- liftToExp $ try parseValueIntExp 
    -- <|>
    -- liftToExp parseValueFloatExp 
    -- <|> parseBoolExp <|> parseVarExp <|> parseColorExp <|> parseText
-- ValueNumExp : ValueNumRaw | ValueNumExpBlock 
-- ValueNumExpBlock : "{" ValueNumExp' "}"
-- ValueNumExp' : "value"" "=" ValueNumExp | ε | ValueNumExp' ValueNumOp("add"...) "=" ValueNumExp
-- chainl 函数可以直接解析左递归的表达式
parseValueIntExp :: ParadoxParser ValueIntExp
parseValueIntExp = parseValueNumExp :: ParadoxParser ValueIntExp
parseValueFloatExp :: ParadoxParser ValueFloatExp
parseValueFloatExp = parseValueNumExp :: ParadoxParser ValueFloatExp
parseUntypedNumExp :: ParadoxParser ValueUntypedNumExp
parseUntypedNumExp = parseValueNumExp :: ParadoxParser ValueUntypedNumExp
parseValueNumExp :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExp = do
    parseValueNumExpBlock <|> parseValueNumRaw
parseValueNumRaw :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumRaw = do
    fmap RawStaticalValue parseNum <|> fmap (RawVar . Var) parseIdentifier <|> fmap RawScriptedValue parseText
parseValueNumExpBlock :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExpBlock = do
    parseReserved "{"
    exp <- parseValueNumExp'
    parseReserved "}"
    return exp
parseAppendValueNumExp :: (ValueNum a) => ParadoxParser (ValueExp a -> ValueExp a)
parseAppendValueNumExp = do
                            let append' op = do 
                                        parseReservedOp "="
                                        exp <- parseValueNumExp
                                        return (\baseExp -> Exp op baseExp exp, pack "")
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
                                            return (id, desc)
                                        )
                            let concatWithDesc (f1, desc1) (f2, desc2) = (f1 . f2, desc1 <> desc2)
                            (f, desc) <- chainl append (return concatWithDesc) (id, pack "")
                            if (DT.null desc) then return f else return (ValueExpWithDesc desc . f)


parseValueNumExp' :: (ValueNum a) => ParadoxParser (ValueExp a)
parseValueNumExp' = do
                        base <- option (RawStaticalValue defaultValue ) (
                                do
                                    parseReserved "value"
                                    parseReservedOp "="
                                    parseValueNumExp
                                )
                        f <- parseAppendValueNumExp
                        return $ f base

