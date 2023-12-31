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
        Right _ -> return ()
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
    --file <- getDataFileName "test/testfile/00_goods.txt"
    file <- getDataFileName "test/testfile/buildings/01_industry.txt"
    handle <- openFile file ReadMode
    hSetEncoding handle utf8_bom 
    input <- DT.IO.hGetContents handle
    let test1 = testCase "unit test1" (assertParsingSuccess (runTestParser parseObjects input))
    defaultMain (testGroup "Our Library Tests" [test1])
    
