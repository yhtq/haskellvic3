module Main where
import Parser(runTestParser, parseObjects)
import Test.Tasty
import Test.Tasty.HUnit
import Paths_vic3_mod
import qualified Data.Text.IO as DT.IO
import Text.Parsec (ParseError)
import System.IO (openFile, IOMode (ReadMode), hSetEncoding, utf8_bom)
import Data.Text (Text)
assertParsingSuccess :: (Show a) => Either ParseError a -> Assertion
assertParsingSuccess expected =
    case expected of
        Left err -> assertFailure $ "Parsing failed: " ++ show err
        -- Right result -> return ()
        Right result -> assertFailure $ "Parsing Finish" ++ show result
genTest :: String -> IO TestTree
genTest path = do
    file <- getDataFileName path
    handle <- openFile file ReadMode
    hSetEncoding handle utf8_bom 
    input <- DT.IO.hGetContents handle
    return $ testCase ("unit test1: " ++ path) (assertParsingSuccess (runTestParser parseObjects input))
main :: IO ()
main = do
    --dir <- getDataDir
    --putStrLn dir
    -- file1 <- getDataFileName "test/testfile/00_goods.txt"
    -- file2 <- getDataFileName "test/testfile/buildings/01_industry.txt"
    test1 <- genTest "test/testfile/00_goods.txt"
    test2 <- genTest "test/testfile/buildings/01_industry.txt"
    test3 <- genTest "test/testfile/buildings/02_agro.txt"
    defaultMain (testGroup "Our Library Tests" [test1, test2, test3])
    
