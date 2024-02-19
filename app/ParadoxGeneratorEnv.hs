{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module ParadoxGeneratorEnv (
    untypedVar,
    EventType(..),
    BaseEvent(..),
    EventEnv,
    newEvent,
    LocalizationMap,
    LocalizationEnv,
    MonadLocalization(..),
    GlobalEnv,
    toLocalSC,
    reverseLocalizationMap
)
where
import Data.Text(Text, pack, justifyLeft, cons, append, singleton, justifyRight)
import GHC.IO.Encoding (getLocaleEncoding)
import Data.Map(Map)
import Data.List(uncons)
import qualified Data.Map as Map
import Control.Monad(when)
import Control.Monad.State (StateT, State, get, put, evalState, evalStateT, runState)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity)
import Text.Show.Unicode (uprint)
import BaseParser hiding (Object(..))

type Event = ObjectInList
type Focus = ObjectInList
focus_tree_x_dis = 0
focus_tree_y_dis = 1
untypedVar :: Identifier -> Var
untypedVar t = Var {
    name = t,
    varType = ""
}

unnameedObject :: [(Key, Exp)] -> ObjectInList
unnameedObject d = ObjectInList {
    obj_name = "",
    declarations = d
}

getId :: ObjectInList -> Var
getId focus = case lookup "id" (declarations focus) of
    Just (FromVar v) -> v
    _ -> error "getId: No id in object"

varToExp :: Var -> Exp
varToExp v = FromVar v

untypedIDToExp :: Text -> Exp
untypedIDToExp t = varToExp $ untypedVar $ textToIdentifier t

updateKV :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
updateKV k v l = map (\(k', v') -> if k' == k then (k, v) else (k', v')) l

-- 在某个 Object 子项中添加一个字段，如果子项不存在则创建，如果子项存在则添加。请不要在有重名 field 时使用
addInField :: ObjectInList -> Key -> (Key, Exp) -> ObjectInList
addInField ori_obj@(ObjectInList _ declarations) field item = case lookup field declarations of
    Nothing -> ori_obj {
        declarations = (field, FromObjectInList $ unnameedObject [item]) : declarations
    }
    Just ori_exp -> case ori_exp of
        FromObjectInList ori@(ObjectInList _ decs) -> ori_obj {
            declarations = updateKV field (FromObjectInList (ori{declarations = item : decs})) decs
        }
        _ -> error "addInField: Not a ObjectInList"
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

-- 这里的 overlap 规则是指出现 LocalizationEnv LocalizationEnv m 时，优先使用内层
instance {-# Overlaps #-} (Monad m) => MonadLocalization (LocalizationEnv m) where
    getLocalizationEnv = get
    putLocalizationEnv = put
instance {-# Overlaps #-} (Monad m, MonadLocalization m) => MonadLocalization (StateT a m) where
    getLocalizationEnv = lift  getLocalizationEnv
    putLocalizationEnv = lift . putLocalizationEnv 
toLocalSC :: (MonadLocalization m) => LocalizationId -> Text -> m ()
toLocalSC id text = toLocal id  [("simp_chinese", text)]

-- 创建事件/国策等对象时默认添加的本地化文本语言
toLocaldefault :: (MonadLocalization m) => LocalizationId -> Text -> m ()
toLocaldefault = toLocalSC

-- 反转本地化映射，用于最终输出
reverseLocalizationMap :: LocalizationMap -> LocalizationMap
reverseLocalizationMap = Map.map reverse
-- 管理事件编号环境, Map 中存储的是当前环境中最大的编号
type NameSpace = Text
type EventEnv  =  StateT (Map NameSpace Integer) GlobalEnv  
data EventType = CountryEvent | NewsEvent
data BaseEvent = BaseEvent {
    namespace :: NameSpace,
    event_type :: EventType,
    -- 以下都是直接的文本
    title :: Text,
    desc :: Text,
    options :: [Text]
}
options_seq :: [Text]
options_seq = map (singleton) ['a'..'z']

-- 生成一个新的事件，自动管理编号问题
newEvent :: BaseEvent -> EventEnv Event
newEvent (BaseEvent namespace event_type title desc options) = do
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
    -- BaseEvent 开头的变量都是指 id, 例如 usa.1, usa.t 之类的
    let event_title = event_name `append` ".t"
    let event_desc = event_name `append` ".d"
    when (length options > (length options_seq))  (error "Too many options")
    let event_options = zipWith (\i t -> event_name `append` "." `append` i) options_seq options
    toLocaldefault event_title title
    toLocaldefault event_desc desc
    sequence $ zipWith toLocaldefault event_options options
    return $ ObjectInList {
        obj_name = event_type_string,
        declarations = [
            ("id", untypedIDToExp event_name),
            ("title", untypedIDToExp event_title),
            ("#", untypedIDToExp title), -- 添加注释
            ("desc", untypedIDToExp event_desc)
        ] ++ (map (\i -> ("option", 
            FromObjectInList ObjectInList {
                obj_name = "",
                declarations = [
                    ("name", untypedIDToExp i)
                ]
            }
        )) event_options)
    }

data BaseFocus = BaseFocus {
    focus_id :: Text,
    focus_name :: Text,   -- 默认语言的国策名
    focus_desc :: Text,   -- 默认语言的国策描述
    focus_ai_will_do :: ObjectInList,
    focus_cost :: Integer,
    focus_cancel_if_invalid :: ConstBoolExp,
    focus_continue_if_invalid :: ConstBoolExp,
    focus_available_if_capitulated :: ConstBoolExp
}

newFocus :: (MonadLocalization m) => BaseFocus -> m Focus
newFocus (BaseFocus id name desc ai_will_do cost cancel_if_invalid continue_if_invalid available_if_capitulated) = do
    toLocaldefault id name
    toLocaldefault (id `append` "_desc") desc
    return $ ObjectInList {
        obj_name = "focus",
        declarations = [
            ("id", untypedIDToExp id),
            ("#", untypedIDToExp name), -- 添加注释
            ("ai_will_do", FromObjectInList ai_will_do),
            ("cost", FromValueIntExp $ RawStaticalValue cost),
            ("cancel_if_invalid", FromConstBoolExp cancel_if_invalid),
            ("continue_if_invalid", FromConstBoolExp continue_if_invalid),
            ("available_if_capitulated", FromConstBoolExp available_if_capitulated)
        ]
    }

data EventFollowedType = EventFollowedType {
    is_hide :: Bool,
    days :: ValueInt
}

newEventFollowingFocus :: EventFollowedType -> Focus -> BaseEvent -> EventEnv Event
newEventFollowingFocus (EventFollowedType is_hide days) focus_obj event = do
    event_obj <- newEvent event
    let event_type_text = case (event_type event) of
            CountryEvent -> "country_event"
            NewsEvent -> "news_event"
    let event_id_var = getId event_obj
    let event_id = name event_id_var
    let Just event_id_exp = lookup "id" (declarations event_obj)
    let effect_type = if is_hide then "hidden_effect" else "complete_reward" 
    return $ addInField focus_obj effect_type (
        event_type_text,
        FromObjectInList $ if not (days == 0) then unnameedObject [
            ("id", event_id_exp),
            ("days", FromValueIntExp $ RawStaticalValue days)
        ] else unnameedObject [
            ("id", event_id_exp)
        ]
        )

-- 添加国策的前置国策
addFocusPrerequisites :: Focus -> [Focus] -> Focus
addFocusPrerequisites focus_obj prerequisites = foldl
     (\acc f -> 
            addInField acc "prerequisite" ("focus", varToExp $ getId f)
        ) focus_obj prerequisites

-- 设置前置国策的同时设置国策位置偏移为 x = focus_tree_x_dis, y = focus_tree_y_dis
setUniquePrerequisites :: Focus -> Focus -> Focus
setUniquePrerequisites focus_obj prerequisite = focus_obj{
    declarations = declarations focus_obj ++ [
        ("x", FromValueIntExp $ RawStaticalValue focus_tree_x_dis),
        ("y", FromValueIntExp $ RawStaticalValue focus_tree_y_dis),
        ("prerequisite", varToExp $ getId prerequisite),
        ("relative_position_id", varToExp $ getId prerequisite)
    ]
}

-- 将若干一条链上的国策连接
lineUpFocus :: [Focus] -> [Focus]
lineUpFocus l = case uncons l of 
    Just (x, xs) -> x : zipWith setUniquePrerequisites xs l
    Nothing -> []
