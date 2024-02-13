{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Parser ()
import Data.Text(Text, pack, justifyLeft, cons, append, singleton, justifyRight)
import GHC.IO.Encoding (getLocaleEncoding)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(when)
import Control.Monad.State (StateT, State, get, put, evalState, evalStateT, runState)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity)
import Text.Show.Unicode (uprint)
import BaseParser
import BasePrinter 
import ParadoxGeneratorEnv
rawInt :: Integer -> Exp
rawInt i = FromValueIntExp $ RawStaticalValue i

test_focus :: ObjectInList
test_focus = ObjectInList {
    obj_name = "focus",
    declarations = [
        ("name", FromVar $ untypedVar "test"),
        ("ai_will_do", FromObjectInList ObjectInList {
            obj_name = "",
            declarations = [
                ("factor", rawInt 10)
            ]
        }),
        ("cost", rawInt 100)
    ] 
}


test_event :: EventEnv ObjectInList
test_event = newEvent $ BaseEvent {
    namespace = "test",
    event_type = CountryEvent,
    title = "测试标题",
    desc = "简介",
    options = ["选项1", "选项2"]
}

test_event2 :: EventEnv [ObjectInList]
test_event2 = sequence $ replicate 2 test_event



main :: IO ()
main = let (event, loc) = runState (evalStateT (test_event2 ) Map.empty) Map.empty in
    do 
        uprint $ map printObjectInList event
        uprint $ reverseLocalizationMap loc

