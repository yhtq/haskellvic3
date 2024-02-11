{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

untypedVar :: Text -> Var
untypedVar t = Var {
    name = t,
    varType = ""
}

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

-- 管理本地化环境
type Language = Text
-- 注意实践上由于效率问题，靠后的条目会在列表前端，因此请在最终打印时使用 reverse 或者 foldr
type LocalizationMap = Map Language [(Text, Text)]
type LocalizationEnv = StateT LocalizationMap
type LocalizationId = Text
type GlobalEnv = LocalizationEnv Identity
class Monad m => MonadLocalization m where
    getLocalizationEnv :: m LocalizationMap
    putLocalizationEnv :: LocalizationMap -> m ()
    -- 登记该 ID 对应的本地化文本，不验证是否重复
    toLocal :: LocalizationId -> Map Language Text ->  m ()
    toLocal id texts = do
        env <- getLocalizationEnv
        let temp = Map.map (\l -> [(id, l)]) texts
        putLocalizationEnv $ Map.unionWith (++) temp env
instance (Monad m) => MonadLocalization (StateT a (LocalizationEnv m)) where
    getLocalizationEnv = lift  get
    putLocalizationEnv = lift . put
toLocalSC :: (MonadLocalization m) => LocalizationId -> Text -> m ()
toLocalSC id text = toLocal id  [("simp_chinese", text)]
reverseLocalizationMap :: LocalizationMap -> LocalizationMap
reverseLocalizationMap = Map.map reverse
-- 管理事件编号环境, Map 中存储的是当前环境中最大的编号
type NameSpace = Text
type EventEnv  =  StateT (Map NameSpace Integer) GlobalEnv  
data EventType = CountryEvent | NewsEvent
data Event = Event {
    namespace :: NameSpace,
    event_type :: EventType,
    -- 以下都是直接的文本
    title :: Text,
    desc :: Text,
    options :: [Text]
}
options_seq :: [Text]
options_seq = map (singleton) ['a'..'z']
-- getEventEnv :: EventEnv (Map NameSpace Integer)
-- getEventEnv = lift get
-- putEventEnv :: Map NameSpace Integer -> EventEnv ()
-- putEventEnv = lift . put
-- 生成一个新的事件，自动管理编号问题
newEvent :: Event -> EventEnv ObjectInList
newEvent (Event namespace event_type title desc options) = do
    env <- get
    let newId = Map.lookup namespace env
    let event_type_string  = case event_type of
                CountryEvent -> "country_event" :: Text
                NewsEvent -> "news_event"
    event_name <- case newId of
        Nothing -> do
            put $ Map.insert namespace 1 env
            return $ (namespace `append` "." `append` (justifyRight 2 '0' $ (pack.show) 1) )
        Just id -> do
            put $ Map.insert namespace (id + 1) env
            return $ (namespace `append` "." `append` (justifyRight 2 '0' $ (pack.show) (id + 1)) )
    -- event 开头的变量都是指 id, 例如 usa.1, usa.t 之类的
    let event_title = event_name `append` ".t"
    let event_desc = event_name `append` ".d"
    when (length options > (length options_seq))  (error "Too many options")
    let event_options = zipWith (\i t -> event_name `append` "." `append` i) options_seq options
    toLocalSC event_title title
    toLocalSC event_desc desc
    sequence $ zipWith toLocalSC event_options options
    return $ ObjectInList {
        obj_name = event_type_string,
        declarations = [
            ("id", FromVar $ untypedVar event_name),
            ("title", FromVar $ untypedVar event_title),
            ("desc", FromVar $ untypedVar event_desc)
        ] ++ (map (\i -> ("option", 
            FromObjectInList ObjectInList {
                obj_name = "",
                declarations = [
                    ("name", FromVar $ untypedVar i)
                ]
            }
        )) event_options)
    }
            
test_event :: EventEnv ObjectInList
test_event = newEvent $ Event {
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

