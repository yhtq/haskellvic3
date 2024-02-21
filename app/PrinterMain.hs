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


test_event :: GlobalEnv CountryEvent
test_event = newEvent $ BaseEvent {
    namespace = "test",
    title = "测试标题",
    desc = "简介",
    options = ["选项1", "选项2"]
}

test_events2 :: GlobalEnv [NewsEvent]
test_events2 = sequence $ replicate 4 (newEvent $ BaseEvent {
    namespace = "test_news",
    title = "测试标题",
    desc = "简介",
    options = ["选项1", "选项2"]
})

default_base_focus :: BaseFocus
default_base_focus = BaseFocus {
    focus_id = "test_focus",
    focus_name = "测试国策",
    focus_desc = "测试国策描述",
    focus_ai_will_do = ai_will_do_single_factor 10,
    focus_cost = 3,
    focus_cancel_if_invalid = BoolRaw Yes,
    focus_continue_if_invalid = BoolRaw No,
    focus_available_if_capitulated = BoolRaw Yes
}

test_focus1 :: GlobalEnv Focus
test_focus1 = newFocus $ default_base_focus{
    focus_id = "test_focus1",
    focus_name = "测试国策1",
    focus_desc = "测试国策描述1",
    focus_cost = 3
}

test_focus2 :: GlobalEnv Focus
test_focus2 = newFocus $ default_base_focus{
    focus_id = "test_focus2",
    focus_name = "测试国策2",
    focus_desc = "测试国策描述2",
    focus_cost = 3
}

test_focus3 :: GlobalEnv Focus
test_focus3 = newFocus $ default_base_focus{
    focus_id = "test_focus3",
    focus_name = "测试国策3",
    focus_desc = "测试国策描述3",
    focus_cost = 3
}

focus3_finish_event :: BaseEvent
focus3_finish_event = BaseEvent {
    namespace = "chi",
    title = "测试国策3完成",
    desc = "测试国策3完成描述",
    options = ["选项1", "选项2"]
}


final_tree :: GlobalEnv ([Focus], [CountryEvent], [NewsEvent])
final_tree = do
    focus1 <- test_focus1
    focus2 <- test_focus2
    focus3 <- test_focus3
    (focus3', event) <- newEventFollowingFocus @CountryEvent (EventFollowedType False 10) focus3 focus3_finish_event
    test_event'<- test_event
    test_events2' <- test_events2
    return (lineUpFocus [focus1, focus2, focus3'], [event, test_event'], test_events2')

emptyInitState :: InitGlobalState
emptyInitState = (Map.empty, Map.empty)
main :: IO ()
main = let (texts, loc) = runPrintFocusTree emptyInitState (FocusTree final_tree) in do
    sequence_ $ map uprint texts
    sequence_ $ map uprint $ Map.toList loc
