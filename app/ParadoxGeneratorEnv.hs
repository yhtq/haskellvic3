{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ParadoxGeneratorEnv (
    Event,
    Focus,
    untypedVar,
    BaseEvent(..),
    GlobalEnv,
    ParadoxObject(..),
    newEvent,
    LocalizationMap,
    LocalizationEnv,
    MonadLocalization(..),
    toLocalSC,
    reverseLocalizationMap,
    newFocus,
    newEventFollowingFocus,
    addFocusPrerequisites,
    setUniquePrerequisites,
    lineUpFocus,
    BaseFocus(..),
    EventFollowedType(..),
    printGlobalEnv,
    InitGlobalState,
    runPrintFocusTree,
    FocusTree(..)
)
where
import Data.Text(Text, pack, justifyLeft, cons, append, singleton, justifyRight)
import GHC.IO.Encoding (getLocaleEncoding)
import Data.Map(Map)
import Data.List(uncons)
import BasePrinter
import qualified Data.Map as Map
import Control.Monad(when)
import Control.Monad.State (StateT, State, get, put, evalState, evalStateT, runState)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity)
import Text.Show.Unicode (uprint)
import Prettyprinter (Doc, Pretty(pretty), (<+>), indent, line, lbrace, rbrace, vsep, equals, dquotes)
import BaseParser hiding (Object(..))

newtype CountryEvent = CountryEvent ObjectInList
newtype NewsEvent = NewsEvent ObjectInList
newtype Focus = Focus ObjectInList
focus_tree_x_dis = 0
focus_tree_y_dis = 1
untypedVar :: Identifier -> Var
untypedVar t = Var {
    name = t,
    varType = ""
}

untypedVarText :: Text -> Var
untypedVarText = untypedVar . textToIdentifier

__getId :: ObjectInList -> Var
__getId focus = case lookup "id" (declarations focus) of
    Just (FromVar v) -> v
    _ -> error "getID: No id in object"
-- 在某个 Object 的 Object 子项中添加一个字段，如果子项不存在则创建，如果子项存在则添加。请不要在有重名子项时使用
__addInField :: ObjectInList -> Key -> (Key, Exp) -> ObjectInList
__addInField ori_obj@(ObjectInList _ declarations) field item = case lookup field declarations of
    Nothing -> ori_obj {
        declarations =  declarations ++ [(field, FromObjectInList $ unnameedObject [item])]
    }
    Just ori_exp -> case ori_exp of
        FromObjectInList ori@(ObjectInList _ decs) -> ori_obj {
            declarations = updateKV field (FromObjectInList (ori{declarations = item : decs})) decs
        }
        _ -> error "addInField: Not a ObjectInList"
        
class ParadoxObject a where
    fromObjectInList :: ObjectInList -> a
    toObjectInList :: a -> ObjectInList
    -- 从 ID 生成一个对象
    newObjFromId :: Var -> a
    getID :: a -> Var
    getID = __getId . toObjectInList
    appendDeclaration ::  (ParadoxObject a) => a -> (Key, Exp) -> a
    appendDeclaration obj (k, v) = fromObjectInList $ (toObjectInList obj) {
        declarations = declarations (toObjectInList obj) ++ [(k, v)]
    }
    appendDeclarations ::  (ParadoxObject a) => a -> [(Key, Exp)] -> a
    appendDeclarations obj l = fromObjectInList $ (toObjectInList obj) {
        declarations = declarations (toObjectInList obj) ++ l
    }
    addInField ::  a -> Key -> (Key, Exp) -> a
    addInField obj field item = fromObjectInList $ __addInField (toObjectInList obj) field item
    lookupField :: (ParadoxObject a) => a -> Key -> Maybe Exp
    lookupField obj field = lookup field (declarations $ toObjectInList obj)
    lookupFields :: (ParadoxObject a) => a -> Key -> [Exp]
    lookupFields obj field = let l1 = filter (\(k, v) -> k == field) (declarations $ toObjectInList obj) in
        map snd l1
    -- 注意这会更新所有同名字段，若字段不存在则不会更新
    modifyField :: (ParadoxObject a) => a -> Key -> (Exp -> Exp) -> a
    modifyField obj field f = fromObjectInList $ (toObjectInList obj) {
        declarations = map (\(k, v) -> if k == field then (k, f v) else (k, v)) (declarations $ toObjectInList obj)
    }
    -- 注意这会更新所有同名字段，若字段不存在则添加
    updateField :: (ParadoxObject a) => a -> Key -> Exp -> a
    updateField obj field exp = case lookup field (declarations $ toObjectInList obj) of
        Just _ -> modifyField obj field (const exp)
        Nothing -> appendDeclaration obj (field, exp)
instance (ParadoxObject a) => ParadoxPrintable a where 
    printParadox obj = printParadox $ toObjectInList obj

instance ParadoxObject Focus where 
    fromObjectInList = Focus
    toObjectInList (Focus obj) = obj
    newObjFromId id = Focus $ ObjectInList {
        obj_name = "focus",
        declarations = [("id", varToExp id)]
    }

class (ParadoxObject a) => Event a where
    event_type_text :: Text 

instance ParadoxObject CountryEvent where 
    fromObjectInList = CountryEvent
    toObjectInList (CountryEvent obj) = obj
    newObjFromId id = CountryEvent $ ObjectInList {
        obj_name = "country_event",
        declarations = [("id", varToExp id)]
    }

instance ParadoxObject NewsEvent where
    fromObjectInList = NewsEvent
    toObjectInList (NewsEvent obj) = obj
    newObjFromId id = NewsEvent $ ObjectInList {
        obj_name = "news_event",
        declarations = [("id", varToExp id)]
    }

instance Event CountryEvent where
    event_type_text = "country_event"
instance Event NewsEvent where
    event_type_text = "news_event"

unnameedObject :: [(Key, Exp)] -> ObjectInList
unnameedObject d = ObjectInList {
    obj_name = "",
    declarations = d
}

objectExp :: [(Key, Exp)] -> Exp
objectExp = FromObjectInList . unnameedObject

lookupID :: (ParadoxObject a) => Var -> [a] -> Maybe a
lookupID id = foldl (\acc f -> if getID f == id then Just f else acc) Nothing

updateID :: (ParadoxObject a) => Var -> (a -> a) -> [a] -> [a]
updateID id f l = case lookupID id l of
    Just focus -> map (\f' -> if getID f' == id then f focus else f') l
    Nothing -> l

varToExp :: Var -> Exp
varToExp v = FromVar v

untypedIDToExp :: Text -> Exp
untypedIDToExp t = varToExp $ untypedVar $ textToIdentifier t

updateKV :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
updateKV k v l = map (\(k', v') -> if k' == k then (k, v) else (k', v')) l

-- 管理本地化环境
type Language = Text
-- 注意实践上由于效率问题，靠后的条目会在列表前端，因此请在最终打印时使用 reverse 或者 foldr
type LocalizationMap = Map Language [(Text, Text)]
type LocalizationEnv = StateT LocalizationMap
type LocalizationId = Text
type Localization = LocalizationEnv Identity
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
type GlobalEnv  =  StateT (Map NameSpace Integer) Localization  
data BaseEvent = BaseEvent {
    namespace :: NameSpace,
    -- 以下都是直接的文本
    title :: Text,
    desc :: Text,
    options :: [Text]
}
options_seq :: [Text]
options_seq = map (singleton) ['a'..'z']

-- 生成一个新的事件，自动管理编号问题，具体类型请用 @ 指定
newEvent :: (Event a) => BaseEvent -> GlobalEnv a
newEvent (BaseEvent namespace title desc options) = do
    env <- get
    let newId = Map.lookup namespace env
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
    let base_obj = newObjFromId  $ untypedVarText event_name
    let obj1 = appendDeclarations base_obj [
            ("id", untypedIDToExp event_name),
            ("title", untypedIDToExp event_title),
            ("#", untypedIDToExp title), -- 添加注释
            ("desc", untypedIDToExp event_desc)
            ]
    return $ appendDeclarations obj1 (map (\i -> ("option", 
            objectExp [("name", untypedIDToExp i)]
        )) event_options)

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

ai_will_do_single_factor :: Integer -> ObjectInList
ai_will_do_single_factor factor = unnameedObject [ ("factor", FromValueIntExp $ RawStaticalValue factor)]

newFocus :: (MonadLocalization m) => BaseFocus -> m Focus
newFocus (BaseFocus id name desc ai_will_do cost cancel_if_invalid continue_if_invalid available_if_capitulated) = do
    toLocaldefault id name
    toLocaldefault (id `append` "_desc") desc
    return $ (newObjFromId $ untypedVarText id) `appendDeclarations` [
        ("name", untypedIDToExp id),
        ("#", untypedIDToExp name), -- 添加注释
        ("desc", untypedIDToExp $ id `append` "_desc"),
        ("ai_will_do", FromObjectInList ai_will_do),
        ("cost", FromValueIntExp $ RawStaticalValue cost),
        ("cancel_if_invalid", FromConstBoolExp cancel_if_invalid),
        ("continue_if_invalid", FromConstBoolExp continue_if_invalid),
        ("available_if_capitulated", FromConstBoolExp available_if_capitulated)
        ]

data EventFollowedType = EventFollowedType {
    is_hide :: Bool,
    days :: ValueInt
}

-- 类似的请用 @ 指定具体生成事件类型
newEventFollowingFocus :: forall a. (Event a) => EventFollowedType -> Focus -> BaseEvent -> GlobalEnv (Focus, a)
newEventFollowingFocus (EventFollowedType is_hide days) focus_obj event = do
    event_obj <- newEvent event
    let event_id = getID event_obj
    let effect_type = if is_hide then "hidden_effect" else "complete_reward" 
    return (addInField focus_obj effect_type (
        textToIdentifier $ event_type_text @a,
        FromObjectInList $ if not (days == 0) then unnameedObject [
            ("id", varToExp event_id),
            ("days", FromValueIntExp $ RawStaticalValue days)
        ] else unnameedObject [
            ("id", varToExp event_id)
        ]
        ), event_obj)

-- 添加国策的前置国策
addFocusPrerequisites :: Focus -> [Focus] -> Focus
addFocusPrerequisites focus_obj prerequisites = foldl
     (\acc f -> 
            addInField acc "prerequisite" ("focus", varToExp $ getID f)
        ) focus_obj prerequisites

-- 设置前置国策的同时设置国策位置偏移为 x = focus_tree_x_dis, y = focus_tree_y_dis
setUniquePrerequisites :: Focus -> Focus -> Focus
setUniquePrerequisites focus_obj prerequisite = 
    let obj = updateField focus_obj "prerequisite" (objectExp [("focus", varToExp $ getID prerequisite)])
    in 
        obj `appendDeclarations` [
        ("x", FromValueIntExp $ RawStaticalValue focus_tree_x_dis),
        ("y", FromValueIntExp $ RawStaticalValue focus_tree_y_dis),
        ("prerequisite", varToExp $ getID prerequisite),
        ("relative_position_id", varToExp $ getID prerequisite)
    ]


-- 将若干一条链上的国策连接
lineUpFocus :: [Focus] -> [Focus]
lineUpFocus l = case uncons l of 
    Just (x, xs) -> x : zipWith setUniquePrerequisites xs l
    Nothing -> []

newtype FocusTree = FocusTree (GlobalEnv ([Focus], [CountryEvent], [NewsEvent]))
printFocusTreeM :: FocusTree -> GlobalEnv [Doc Text]
printFocusTreeM (FocusTree m) = do
    (focus, country_events, news_events) <- m
    return [printParadox focus, printParadox country_events, printParadox news_events]

printLocalizationMap :: LocalizationMap -> Map Language (Doc Text)
printLocalizationMap loc = Map.map (vsep . map (\(k, v) -> pretty k <+> equals <+> dquotes (pretty v))) (reverseLocalizationMap loc)

type InitGlobalState = (Map NameSpace Integer, LocalizationMap)
printGlobalEnv :: InitGlobalState -> GlobalEnv [Doc Text] -> ([Doc Text], Map Language (Doc Text))
printGlobalEnv (event_env, loc_env) m = let (rest, loc) = runState (evalStateT m event_env) loc_env in 
    (rest,  printLocalizationMap loc)

runPrintFocusTree :: InitGlobalState -> FocusTree -> ([Doc Text], Map Language (Doc Text))
runPrintFocusTree init_state tree = printGlobalEnv init_state (printFocusTreeM tree)