module Main where
import Parser (defaultKeyParserMap, parseDeclaration, parseExp, parseObjects, runTestParser)
import UnTypedNumExpParser(parseValueFloatExp, parseValueIntExp, parseValueUntypedNumExp)
import BaseParser
import Test.Tasty
import Test.Tasty.HUnit
import Paths_vic3_mod
import qualified Data.Text.IO as DT.IO
import Text.Megaparsec 
import Text.Megaparsec.Error
import System.IO (openFile, IOMode (ReadMode), hSetEncoding, utf8_bom)
testInt :: Assertion
testInt = do
    case (runTestParser () parseValueIntExp "1") of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            RawStaticalValue 1 -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
    case (runTestParser () (optionMaybe parseInt) "1.0") of
        Left err -> assertFailure $ errorBundlePretty err
        Right Nothing -> return ()
        _ -> assertFailure $ "Parsing failed"
    
testFloat :: Assertion
testFloat = case (runTestParser () parseValueFloatExp "1.0") of
    Left err -> assertFailure $ errorBundlePretty err
    Right result -> case result of
        RawStaticalValue 1.0 -> return ()
        _ -> assertFailure $ "Parsing failed: " ++ show result
testUntyped :: Assertion
testUntyped = do
    let res1 = runTestParser () parseValueUntypedNumExp "1"
    case res1 of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            (RawStaticalValue (ValueInt 1), Int) -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
    let res2 = runTestParser () parseValueUntypedNumExp "1.0"
    case res2 of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            (RawStaticalValue (ValueFloat 1.0), Float) -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
    let res3 = runTestParser () parseValueUntypedNumExp "-1.0"
    case res3 of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            (RawStaticalValue (ValueFloat (-1.0)), Float) -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
    let res6 = runTestParser () (many $ parseDeclaration defaultKeyParserMap) "traded_quantity = 3 traded_quantity = 3.5 "
    case res6 of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            [Declaration (Text "traded_quantity") (FromValueIntExp (RawStaticalValue  3)), Declaration (Text "traded_quantity") (FromValueFloatExp (RawStaticalValue 3.5))] -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
testLiteral :: Assertion
testLiteral = do
    let res1 = runTestParser () (optionMaybe parseIdentifier) "abc_123"
    case res1 of
        Left err -> assertFailure $ errorBundlePretty err
        Right result -> case result of
            Just (Text "abc_123") -> return ()
            _ -> assertFailure $ "Parsing failed: " ++ show result
    let res2 = runTestParser () (optionMaybe parseIdentifier) "1abc_123"
    case res2 of
        Right Nothing -> return ()
        _ -> assertFailure $ "Parsing failed"

assertParsingSuccess :: (Show a) => (Either (ParseErrorBundle TokenType ErrorType) a) -> Assertion
assertParsingSuccess expected =
    case expected of
        Left err -> assertFailure $ "Parsing failed: " ++ errorBundlePretty err
        -- Right result -> return ()
        Right result -> assertFailure $ "Parsing Finish" ++ show result
genTest :: String -> IO TestTree
genTest path = do
    file <- getDataFileName path
    handle <- openFile file ReadMode
    hSetEncoding handle utf8_bom 
    input <- DT.IO.hGetContents handle
    return $ testCase ("unit test1: " ++ path) (assertParsingSuccess (runTestParser () parseObjects input))
main :: IO ()
main = do
    --dir <- getDataDir
    --putStrLn dir
    -- file1 <- getDataFileName "test/testfile/00_goods.txt"
    -- file2 <- getDataFileName "test/testfile/buildings/01_industry.txt"
    test1 <- genTest "test/testfile/00_goods.txt"
    test2 <- genTest "test/testfile/buildings/01_industry.txt"
    test3 <- genTest "test/testfile/buildings/02_agro.txt"
    defaultMain (testGroup "Our Library Tests" [
        testCase "Int test" testInt,
        testCase "Float test" testFloat,
        testCase "Untyped Test" testUntyped, 
        testCase "literal" testLiteral, 
        test1, test2, test3])
    
