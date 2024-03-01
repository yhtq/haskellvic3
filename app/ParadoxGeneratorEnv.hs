{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ParadoxGeneratorEnv (
    CountryEvent(..),
    NewsEvent(..),
    Focus,
    BaseEvent(..),
    EventEnv,
    ParadoxObject(..),
    LocalizationMap,
    LocalizationEnvT,
    MonadLocalization(..),
    toLocalSC,
    reverseLocalizationMap,
    BaseFocus(..),
    EventFollowedType(..),
    Utilities(..),
    emptyEventState,
    emptyPrintState,
    printObjects,
    printObjectsM,
    Term(..),
    FileName,
    MonadPrintEnv(..),
    runPrinterWithLocalizationsM,
    PrintEventState,
    FileContentMap,
    liftEventEnvToPrint,
    runPrintEnvEmpty,
    EventTools(..),
    FocusTools(..),

)
where
import Prelude hiding (exp, id)
import Data.Text(Text, pack, append, singleton, justifyRight)
import Data.Map(Map)
import Data.List(uncons)
import BasePrinter ( ParadoxPrintable(..), DocType )
import qualified Data.Map as Map
import Control.Monad(when, liftM)
import Control.Monad.State (StateT, get, put, evalStateT, runState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Identity (Identity, runIdentity)
import Prettyprinter (Doc, Pretty(pretty), (<+>), vsep, equals, dquotes)
import BaseParser hiding (Object(..))


instance Term Dec where
    toExp :: Dec -> Exp
    toExp d = FromObjectInList $ ObjectInList {
        obj_name = "",
        declarations = [d]
    }

instance Term [Dec] where
    toExp :: [Dec] -> Exp
    toExp ds = FromObjectInList $ ObjectInList {
        obj_name = "",
        declarations = ds
    }
class Utilities where
    unnameedObject :: [Dec] -> ObjectInList
    unnameedObject d = ObjectInList {
        obj_name = "",
        declarations = d
    }

    untypedIDToExp :: Text -> Exp
    untypedIDToExp t = toExp $ untypedVar $ textToIdentifier t

    updateKV :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
    updateKV k v l = map (\(k', v') -> if k' == k then (k, v) else (k', v')) l
    
    -- 考虑到 Var/Text 可能产生歧义，不能直接 instance Term Var
    untypedVar :: Identifier -> Var
    untypedVar t = Var {
        name = t,
        varType = ""
    }

    untypedVarText :: Text -> Var
    untypedVarText = untypedVar . textToIdentifier
    

instance Utilities

newtype CountryEvent = CountryEvent ObjectInList
newtype NewsEvent = NewsEvent ObjectInList
newtype Focus = Focus ObjectInList
focus_tree_x_dis :: ValueInt
focus_tree_x_dis = 0
focus_tree_y_dis :: ValueInt
focus_tree_y_dis = 1

__getId :: Identifier -> ObjectInList -> Var
__getId key obj = case lookup key (declarations obj) of
    Just (FromVar v) -> v
    _ -> error ("getID: No " ++ show key ++ "in obj")
-- 在某个 Object 的 Object 子项中添加一个字段，如果子项不存在则创建，如果子项存在则添加。请不要在有重名子项时使用
__addsInField :: ObjectInList -> Key -> [Dec] -> ObjectInList
__addsInField ori_obj@(ObjectInList _ declarations) field item = case lookup field declarations of
    Nothing -> ori_obj {
        declarations =  declarations ++ [(field, toExp item)]
    }
    Just ori_exp -> case ori_exp of
        FromObjectInList ori_field@(ObjectInList _ field_decs) -> ori_obj {
            declarations = updateKV field (FromObjectInList (ori_field{declarations = field_decs ++ item})) declarations
        }
        _ -> error "addInField: Not a ObjectInList"
        
class ParadoxObject a where
    fromObjectInList :: ObjectInList -> a
    toObjectInList :: a -> ObjectInList
    -- 该类型对象声明中的名称，例如 focus, country_event
    obj_dec_name :: TokenType
    -- id 字段名称
    idFieldName :: Identifier
    -- 从 ID 生成一个对象，自动填充 ID 字段
    newObjFromId :: Var -> a
    newObjFromId id = fromObjectInList ObjectInList {
        obj_name = obj_dec_name @a,
        declarations = [(idFieldName @a, toExp id)]
    }
    getID :: a -> Var
    getID = __getId (idFieldName @a) . toObjectInList
    appendDeclaration ::  (ParadoxObject a) => a -> Dec -> a
    appendDeclaration obj (k, v) = fromObjectInList $ (toObjectInList obj) {
        declarations = declarations (toObjectInList obj) ++ [(k, v)]
    }
    appendDeclarations ::  (ParadoxObject a) => a -> [Dec] -> a
    appendDeclarations obj l = fromObjectInList $ (toObjectInList obj) {
        declarations = declarations (toObjectInList obj) ++ l
    }
    addInField ::  a -> Key -> Dec -> a
    addInField obj field item = fromObjectInList $ __addsInField (toObjectInList obj) field [item]
    lookupField :: (ParadoxObject a) => a -> Key -> Maybe Exp
    lookupField obj field = lookup field (declarations $ toObjectInList obj)
    lookupFields :: (ParadoxObject a) => a -> Key -> [Exp]
    lookupFields obj field = let l1 = filter (\(k, _) -> k == field) (declarations $ toObjectInList obj) in
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
    
    addsInField :: (ParadoxObject a) => a -> Key -> [Dec] -> a
    addsInField obj field items = fromObjectInList $ __addsInField (toObjectInList obj) field items

instance {-# OVERLAPPABLE #-}(ParadoxObject a) => ParadoxPrintable a where 
    printParadox obj = printParadox $ toObjectInList obj

instance ParadoxObject Focus where 
    fromObjectInList = Focus
    toObjectInList (Focus obj) = obj
    obj_dec_name = "focus"
    idFieldName = "name"

class (ParadoxObject a) => Event a where
    event_type_text :: Text 

instance ParadoxObject CountryEvent where 
    fromObjectInList = CountryEvent
    toObjectInList (CountryEvent obj) = obj
    obj_dec_name = "country_event"
    idFieldName = "id"

instance ParadoxObject NewsEvent where
    fromObjectInList = NewsEvent
    toObjectInList (NewsEvent obj) = obj
    obj_dec_name = "news_event"
    idFieldName = "id"

instance Event CountryEvent where
    event_type_text = "country_event"
instance Event NewsEvent where
    event_type_text = "news_event"


objectExp :: [Dec] -> Exp
objectExp = FromObjectInList . unnameedObject

lookupID :: (ParadoxObject a) => Var -> [a] -> Maybe a
lookupID id = foldl (\acc f -> if getID f == id then Just f else acc) Nothing

updateID :: (ParadoxObject a) => Var -> (a -> a) -> [a] -> [a]
updateID id f l = case lookupID id l of
    Just focus -> map (\f' -> if getID f' == id then f focus else f') l
    Nothing -> l


-- 管理本地化环境
type Language = Text
-- 注意实践上由于效率问题，靠后的条目会在列表前端，因此请在最终打印时使用 reverse 或者 foldr
newtype LocalizationMap = LocalizationMap (Map Language [(Text, Text)]) deriving (Show, Eq)
type LocalizationEnvT = StateT LocalizationMap
type LocalizationId = Text
type Localization = LocalizationEnvT Identity
class Monad m => MonadLocalization m where
    getLocalizationEnvT :: m LocalizationMap
    putLocalizationEnvT :: LocalizationMap -> m ()
    -- 登记该 ID 对应的本地化文本，不验证是否重复
    toLocal :: LocalizationId -> Map Language Text ->  m ()
    toLocal id texts = do
        LocalizationMap env <- getLocalizationEnvT
        let temp = Map.map (\l -> [(id, l)]) texts
        putLocalizationEnvT $ LocalizationMap  (Map.unionWith (++) temp env)

-- 这里的 overlap 规则是指出现 LocalizationEnvT LocalizationEnvT m 时，优先使用内层
instance {-# OVERLAPPABLE  #-} (Monad m) => MonadLocalization (LocalizationEnvT m) where
    getLocalizationEnvT = get
    putLocalizationEnvT = put
instance {-# Overlaps #-} (MonadTrans m1, MonadLocalization m) => MonadLocalization (m1 m) where
    getLocalizationEnvT = lift  getLocalizationEnvT
    putLocalizationEnvT = lift . putLocalizationEnvT 
toLocalSC :: (MonadLocalization m) => LocalizationId -> Text -> m ()
toLocalSC id text = toLocal id  [("simp_chinese", text)]

-- 创建事件/国策等对象时默认添加的本地化文本语言
toLocaldefault :: (MonadLocalization m) => LocalizationId -> Text -> m ()
toLocaldefault = toLocalSC

-- 反转本地化映射，用于最终输出
reverseLocalizationMap :: LocalizationMap -> LocalizationMap
reverseLocalizationMap (LocalizationMap m) = LocalizationMap $ Map.map reverse m


-- 管理事件编号环境, Map 中存储的是当前环境中最大的编号
type NameSpace = Text
newtype EventNumbering = EventNumbering (Map NameSpace Integer) deriving (Show, Eq)
type EventEnvT = StateT EventNumbering
class (Monad m) => MonadEventEnv m where
    getEventEnv :: m EventNumbering
    putEventEnv :: EventNumbering -> m ()
    lookupNamespace :: NameSpace -> m (Maybe Integer)
    lookupNamespace ns = do
        EventNumbering env <- getEventEnv
        return $ Map.lookup ns env
    insertInNamespace :: NameSpace -> Integer -> m ()
    insertInNamespace ns id = do
        EventNumbering env <- getEventEnv
        putEventEnv $ EventNumbering $ Map.insert ns id env
instance {-# OVERLAPPABLE #-} (Monad m) => MonadEventEnv (EventEnvT m) where
    getEventEnv = get
    putEventEnv = put
instance {-# Overlaps #-}  (MonadEventEnv m, MonadTrans m1) => MonadEventEnv (m1 m) where
    getEventEnv = lift getEventEnv
    putEventEnv = lift . putEventEnv


-- 管理打印环境。请注意一旦 append 到打印环境中，可修改的对象便会变成文本，同时无法索引，因此必须以最终状态进入
type FileName = Text
newtype FileContentMap = FileContentMap (Map FileName DocType)
type PrintEnvT = StateT FileContentMap
concatNewLine :: DocType -> DocType -> DocType
concatNewLine a b = vsep [a, b]
concatNewLineRevert :: DocType -> DocType -> DocType
concatNewLineRevert a b = vsep [b, a]
class (Monad m) => MonadPrintEnv m where
    getPrintEnv :: m FileContentMap
    putPrintEnv :: FileContentMap -> m ()
    append_in_file :: forall a. (ParadoxPrintable a) => FileName -> a -> m ()
    append_in_file file obj = do
        FileContentMap env <- getPrintEnv
        let doc = printParadox obj
        putPrintEnv $ FileContentMap $ Map.insertWith concatNewLineRevert file doc env
instance {-# OVERLAPPABLE #-} (Monad m) => MonadPrintEnv (PrintEnvT m) where
    getPrintEnv = get
    putPrintEnv = put
instance {-# Overlaps #-}  (MonadPrintEnv m, MonadTrans m1) => MonadPrintEnv (m1 m) where
    getPrintEnv = lift getPrintEnv
    putPrintEnv = lift . putPrintEnv

printLoc :: [(Text, Text)] -> DocType
printLoc = vsep . map (\(k, v) -> pretty k <+> equals <+> dquotes (pretty v))
-- 这将与本地化文本一起打印，输入函数为各个语言的本地化文本文件名。方便起见，这里的返回值就不包了
runPrinterWithLocalizationsM :: (MonadLocalization m, MonadPrintEnv m) => (Language -> FileName) -> m (Map FileName DocType)
runPrinterWithLocalizationsM f = do
    loc <- getLocalizationEnvT
    FileContentMap other <- getPrintEnv
    let LocalizationMap loc' = reverseLocalizationMap loc
    let loc'' = Map.mapKeysWith (<>) f loc'
    return $ Map.unionWith (concatNewLine) other $ Map.map printLoc loc''

-- 
type EventEnv  =  EventEnvT ( LocalizationEnvT Identity)
type PrintEventState = PrintEnvT EventEnv
emptyLocalizationMap :: LocalizationMap
emptyLocalizationMap = LocalizationMap Map.empty
emptyEventState :: EventNumbering
emptyEventState = EventNumbering Map.empty
emptyPrintState :: FileContentMap
emptyPrintState = FileContentMap Map.empty
liftEventEnvToPrint :: EventEnv a -> PrintEventState a
liftEventEnvToPrint = lift 
runPrintEnvEmpty :: PrintEventState a -> a
runPrintEnvEmpty m = let m' = evalStateT m emptyPrintState in
    let m'' = evalStateT m' emptyEventState in
        let m''' = evalStateT m'' emptyLocalizationMap in
            runIdentity m'''
type EventOption = (Text, [Dec])
data BaseEvent = BaseEvent {
    namespace :: NameSpace,
    -- 以下都是直接的文本
    title :: Text,
    desc :: Text,
    is_triggered_only  :: Bool,
    fire_only_once :: Bool,
    trigger :: [Dec],
    options :: [EventOption]
}
options_seq :: [Text]
options_seq = map singleton ['a'..'z'] ++ map (pack.show) [1..]

data EventFollowedType = EventFollowedType {
    is_hide :: Bool,
    fire_decs :: [Dec]
}

class EventTools where
    -- 生成一个新的事件，自动管理编号问题，具体类型请用 @ 指定
    newEvent :: (Event a, MonadEventEnv m, MonadLocalization m) => BaseEvent -> m a
    newEvent (BaseEvent namespace title desc is_triggered_only fire_only_once trigger options) = do
        newId <- lookupNamespace namespace 
        event_name <- case newId of
            Nothing -> do
                insertInNamespace namespace 1
                return $ (namespace `append` "." `append` (justifyRight 2 '0' $ (pack.show) 1) )
            Just id -> do
                -- put $ Map.insert namespace (id + 1) env
                insertInNamespace namespace (id + 1)
                return $ (namespace `append` "." `append` (justifyRight 2 '0' $ (pack.show) (id + 1)) )
        -- BaseEvent 开头的变量都是指 id, 例如 usa.1, usa.t 之类的
        let event_title = event_name `append` ".t"
        let event_desc = event_name `append` ".d"
        toLocaldefault event_title title
        toLocaldefault event_desc desc
        let base_obj = newObjFromId  $ untypedVarText event_name
        let obj1 = appendDeclarations base_obj ([
                ("title", untypedIDToExp event_title),
                ("#", untypedIDToExp title), -- 添加注释
                ("desc", untypedIDToExp event_desc)
                ] ++
                if null trigger then [] else [("trigger", toExp trigger)] ++
                if is_triggered_only then [("is_triggered_only" , toExp Yes)] else [] ++
                if fire_only_once then [("fire_only_once", toExp Yes)] else []
                )
        -- _ <- sequence $ zipWith toLocaldefault event_options_name (map (\(i, _) -> i) options)
        -- return $ appendDeclarations obj1 (map (\(id, decs) -> ("option", 
        --         objectExp $ [("name", untypedIDToExp id)] ++ decs
        --     )) options')
        appendOptions obj1 options

    appendOptions :: (Event a, MonadLocalization m) => a -> [EventOption] -> m a
    appendOptions e options = do
        let ori_options = lookupFields e "option"
        let ori_options_id = map (\(FromObjectInList (ObjectInList _ decs)) -> identifierToText $ name $ case lookup "name" decs of
                    Just (FromVar v) -> v
                    _ -> error "appendOptions: No name in option"
                ) ori_options
        let event_id = identifierToText $ name $ getID e
        -- 类似 usa.1.a, usa.1.b
        let option_seq' = map (\i -> event_id `append` "." `append` i) options_seq
        let event_options_id = takeWhile (\i -> i `notElem` ori_options_id) option_seq'
        let event_decs = map (\(_, i) -> i) options
        let options' = zip event_options_id event_decs -- 这是将 title 文本换成 id 后的列表
        _ <- sequence $ zipWith toLocaldefault event_options_id (map (\(i, _) -> i) options) -- 将 title 本本登记到本地化环境
        return $ appendDeclarations e (map (\(id, decs) -> ("option", 
                objectExp $ [("name", untypedIDToExp id)] ++ decs
            )) options')

    event_to_trigger :: forall a. (Event a) => EventFollowedType -> a -> Dec
    event_to_trigger (EventFollowedType is_hide decs) event = 
        let event_id = getID event in
        let trigger_dec = 
                (
                    textToIdentifier $ event_type_text @a,
                    FromObjectInList $  unnameedObject $
                        [("id", toExp event_id)] ++
                        decs 
                ) in
                    if is_hide then ("hidden_effect", FromObjectInList $ unnameedObject [trigger_dec]) else trigger_dec
    
    ai_chance_with_single_factor :: ValueInt -> Dec
    ai_chance_with_single_factor factor = ("ai_chance", FromObjectInList $ unnameedObject [
        ("factor", toExp factor)
        ])
    
instance EventTools

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


class FocusTools where 
    ai_will_do_single_factor :: Integer -> ObjectInList
    ai_will_do_single_factor factor = unnameedObject [ ("factor", FromValueIntExp $ RawStaticalValue factor)]

    newFocus :: (MonadLocalization m) => BaseFocus -> m Focus
    newFocus (BaseFocus id name desc ai_will_do cost cancel_if_invalid continue_if_invalid available_if_capitulated) = do
        toLocaldefault id name
        toLocaldefault (id `append` "_desc") desc
        return $ (newObjFromId $ untypedVarText id) `appendDeclarations` [
            ("#", untypedIDToExp name), -- 添加注释
            ("desc", untypedIDToExp $ id `append` "_desc"),
            ("ai_will_do", FromObjectInList ai_will_do),
            ("cost", FromValueIntExp $ RawStaticalValue cost),
            ("cancel_if_invalid", FromConstBoolExp cancel_if_invalid),
            ("continue_if_invalid", FromConstBoolExp continue_if_invalid),
            ("available_if_capitulated", FromConstBoolExp available_if_capitulated)
            ]

    -- 类似的请用 @ 指定具体生成事件类型
    newEventFollowingFocus :: forall a m. (Event a, MonadEventEnv m, MonadLocalization m) => EventFollowedType -> Focus -> BaseEvent -> m (Focus, a)
    newEventFollowingFocus (EventFollowedType is_hide decs) focus_obj event = do
        event_obj <- newEvent event
        let event_id = getID event_obj
        let effect_type = if is_hide then "hidden_effect" else "completion_reward" 
        return (addInField focus_obj effect_type (
            textToIdentifier $ event_type_text @a,
            toExp $ [
                ("id", toExp event_id)
            ] ++ decs
            ), event_obj)

    -- 添加国策的前置国策
    addFocusPrerequisites :: Focus -> [Focus] -> Focus
    addFocusPrerequisites focus_obj prerequisites = foldl
        (\acc f -> 
                addInField acc "prerequisite" ("focus", toExp $ getID f)
            ) focus_obj prerequisites

    appendRewards :: Focus -> [Dec] -> Focus
    appendRewards focus_obj rewards = addsInField focus_obj "complete_reward" rewards

    -- 设置前置国策的同时设置国策位置偏移为 x = focus_tree_x_dis, y = focus_tree_y_dis
    setUniquePrerequisites :: Focus -> Focus -> Focus
    setUniquePrerequisites focus_obj prerequisite = 
        let obj = updateField focus_obj "prerequisite" (objectExp [("focus", toExp $ getID prerequisite)])
        in 
            obj `appendDeclarations` [
            ("x", FromValueIntExp $ RawStaticalValue focus_tree_x_dis),
            ("y", FromValueIntExp $ RawStaticalValue focus_tree_y_dis),
            ("prerequisite", toExp $ getID prerequisite),
            ("relative_position_id", toExp $ getID prerequisite)
        ]


    -- 将若干一条链上的国策连接
    lineUpFocus :: [Focus] -> [Focus]
    lineUpFocus l = case uncons l of 
        Just (x, xs) -> x : zipWith setUniquePrerequisites xs l
        Nothing -> []
instance FocusTools
-- newtype FocusTree = FocusTree (EventEnv ([Focus], [CountryEvent], [NewsEvent]))
-- printFocusTreeM :: FocusTree -> EventEnv [DocType]
-- printFocusTreeM (FocusTree m) = do
--     (focus, country_events, news_events) <- m
--     return [printParadox focus, printParadox country_events, printParadox news_events]

printObjects :: (ParadoxPrintable a) => [a] -> (DocType)
printObjects = printParadox 

printObjectsM :: (ParadoxPrintable a) => EventEnv [a] -> EventEnv (DocType)
printObjectsM  = liftM printObjects 

printLocalizationMap :: LocalizationMap -> Map Language (DocType)
printLocalizationMap loc = 
    let LocalizationMap r_loc = reverseLocalizationMap loc in
    Map.map (vsep . map (\(k, v) -> pretty k <+> equals <+> dquotes (pretty v))) r_loc


-- printEventEnv :: GlobalGeneratorState -> EventEnv [DocType] -> ([DocType], Map Language (DocType))
-- printEventEnv (event_env, loc_env) m = let (rest, loc) = runState (evalStateT m event_env) loc_env in 
--     (rest,  printLocalizationMap loc)

-- runPrintFocusTree :: GlobalGeneratorState -> FocusTree -> ([DocType], Map Language (DocType))
-- runPrintFocusTree init_state tree = printEventEnv init_state (printFocusTreeM tree)