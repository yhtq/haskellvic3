{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import ParadoxGeneratorEnv
import BaseParser
import Parser
import Text.RawString.QQ
import Data.Text (Text, append, concat, cons, pack, singleton, unpack, intercalate)
import Data.Map (toList, Map)
import Data.List(uncons)
import Prettyprinter(pretty)
import BasePrinter
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Text.Show.Unicode (uprint, ushow)
import Utilities

-- https://stackoverflow.com/questions/58682357/how-to-create-a-file-and-its-parent-directories-in-haskell
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

focus_template :: BaseFocus
focus_template = BaseFocus {
  focus_id = "",
  focus_name = "",   -- 默认语言的国策名
  focus_desc = "",   -- 默认语言的国策描述
  focus_ai_will_do = ai_will_do_single_factor 20,
  focus_cost = 3,
  focus_cancel_if_invalid = BoolRaw Yes,
  focus_continue_if_invalid = BoolRaw No,
  focus_available_if_capitulated = BoolRaw Yes
}

event_template ::BaseEvent
event_template = BaseEvent {
  namespace = "",
  title = "",
  desc = "",
  is_triggered_only = False,
  fire_only_once = True,
  trigger = [],
  options = []
}

event_following_immediate :: EventFollowedType
event_following_immediate = EventFollowedType {
  is_hide = False,
  fire_decs = []
}

genBlackMonday' :: Int -> EventEnv [ObjectInList]
genBlackMonday' n = do 
  toLocalSC "URG_black_monday" "黑色星期一"
  toLocalSC "URG_black_monday_desc" "黑色星期一 debuff"
  sequence (map  genBlackMonday [1..n]) 
    where genBlackMonday n = let name' = "URG_black_monday_" `append` intToText n in 
            do
              return $ ObjectInList {
                obj_name = name',
                declarations = [
                  (
                    "on_add", 
                    toExp ("log" :: Identifier, FromText $ "[GetDateText]: [Root.GetName]: add idea " `append` name')
                  ),
                  (
                    "name",
                    untypedIDToExp "URG_black_monday"
                  ),
                  (
                    "picture", 
                    untypedIDToExp "great_depression"
                  ),
                  (
                    "allowed",
                    toExp $ alwaysDec No
                  ),
                  (
                    "modifier",
                    let nd :: Double = fromIntegral n in toExp 
                      [
                        ("production_factory_max_efficiency_factor" :: Identifier, toExp (-0.05 * nd)),
                        ("production_factory_efficiency_gain_factor" :: Identifier, toExp (-0.05 * nd)),
                        ("production_speed_buildings_factor" :: Identifier, toExp (-0.05 * nd)),
                        ("consumer_goods_factor" :: Identifier, toExp (-0.05 * nd))
                      ] 
                    )
                  ]
              }
genBlackMondayDecrease :: Int -> EventEnv ObjectInList
genBlackMondayDecrease n = return $ ObjectInList{
  obj_name = "URG_recover_from_economic_crisis",
  declarations = genSeqOfIdeaChange (map 
    (\i -> "URG_black_monday_" `append` intToText i)
    $ reverse [1..n]
    )
  }
newFocus' = newFocus @EventEnv
经济大萧条 = (newFocus' $ focus_template {
  focus_id = "URG_economic_crisis",
  focus_name = "经济大萧条",
  focus_desc = "经济大萧条"
}) `add_custom_effect_tooltip_on_focusM1` "unlock_focuses"
抛弃金本位制 = (newFocus' $ focus_template {
  focus_id = "URG_abandon_gold_standard",
  focus_name = "抛弃金本位制",
  focus_desc = "抛弃金本位制"
}) `add_timed_idea_on_focusM1`  ("URG_gold_standard_abolished" :: Identifier) $ 180
抛弃金本位制_idea :: EventEnv ObjectInList
抛弃金本位制_idea = do
  toLocalSC "URG_gold_standard_abolished" "抛弃金本位制"
  return $ ObjectInList {
  obj_name = "URG_gold_standard_abolished",
  declarations = [
    (
      "name",
      untypedIDToExp "URG_gold_standard_abolished"
    ),
    (
      "allowed",
      toExp $ alwaysDec No
    ),
    (
      "modifier",
      toExp [
        ("consumer_goods_factor" :: Identifier, toExp (-0.1 :: ValueFloat))
      ]
    )
  ]
}
recover_from_economic_crisis :: Dec
recover_from_economic_crisis = runParseDec "URG_recover_from_economic_crisis = yes" 
贱卖农产品 = (newFocus' $ focus_template {
  focus_id = "URG_sell_agricultural_products_at_low_price",
  focus_name = "贱卖农产品",
  focus_desc = "贱卖农产品"
}) `add_focus_rewardM1` (runParseDecs [r|
    URG_recover_from_economic_crisis = yes
		add_popularity = {
				ideology = authoritarian_democrat
				popularity = 0.1
		}
    |]
      )  
还有谁要乌拉圭的牛肉 :: EventEnv (Focus, CountryEvent)
还有谁要乌拉圭的牛肉 = do
  f <- newFocus' $ focus_template {
    focus_id = "URG_who_wants_uruguayan_beef",
    focus_name = "还有谁要乌拉圭的牛肉",
    focus_desc = "还有谁要乌拉圭的牛肉",
    focus_cost = 2
    }
  let f' = add_focus_reward (runParseDecs "add_political_power = 100") f
  newEventFollowingFocus @CountryEvent event_following_immediate f' 
    event_template {
      namespace = "URG",
      title = "还有谁要乌拉圭的牛肉",
      desc = "马尼尼不得不对牛肉进行贱卖",
      is_triggered_only = True,
      options = [
        ("完成", [])
      ]
    }
-- 参数表示单个国策对应的国家和它们的要求
卖给_ :: [(Text, [Dec])] -> PrintEventState Focus
卖给_ l = do
  let event_following = EventFollowedType {
    is_hide = False,
    fire_decs = [
      runParseDec "days = 5",
      runParseDec "random_days = 2"
    ]
  }
  let all_country = intercalate "_"  (map fst l)
  f <- newFocus $ focus_template {
    focus_id = "URG_sell_to_" `append` all_country,
    focus_name = "卖给 " `append` all_country,
    focus_desc = "卖给 " `append` all_country,
    focus_cost = 3
    }
  let event_template' = event_template{
    is_triggered_only = True,
    fire_only_once = True
  }
  let event_chain_of_country f1 who condition = do
        e <- newEvent @CountryEvent $ event_template' {
          namespace = "URG",
          title = who `append` "是否提出条件",
          desc = who `append` "是否提出条件",
          options = []
        }
        let f' = add_focus_reward  [
                to_another_scope (textToIdentifier who) [event_to_trigger event_following e]
              ] f1
        e_with_condition <- newEvent @CountryEvent $ event_template' {
          namespace = "URG",
          title = who `append` "提出条件",
          desc = who `append` "提出条件",
          options = []
        }
        e_without_condition <- newEvent @CountryEvent $ event_template' {
          namespace = "URG",
          title = who `append` "不提出条件",
          desc = who `append` "不提出条件",
          options = [
            ("减少经济危机", [recover_from_economic_crisis])
          ]
        }
        e' <- appendOptions e [
          ("提出条件", [to_another_scope "URG" [event_to_trigger event_following e_with_condition]]),
          ("无条件", 
            [
              to_another_scope "URG" [event_to_trigger event_following e_without_condition],
              comment_dec "ai 不会同意无条件",
              ai_chance_with_single_factor 0
              ]
            )
          ]
        e_accept_cond <- newEvent @CountryEvent $ event_template' {
          namespace = "URG",
          title = "乌拉圭接受了条件",
          desc = "乌拉圭接受了条件",
          options = [
            ("乌拉圭接受了条件", [])
          ]
        }
        e_refuse_cond <- newEvent @CountryEvent $ event_template' {
          namespace = "URG",
          title = "乌拉圭拒绝条件",
          desc = "乌拉圭拒绝条件",
          options = [
            ("乌拉圭拒绝条件", [])
          ]
        }
        e_with_condition' <- appendOptions e_with_condition [
          ("接受", 
            [to_another_scope (textToIdentifier who) (
              [event_to_trigger event_following e_accept_cond] ++ condition ++ [recover_from_economic_crisis])
              ]
            ),
          ("拒绝", 
            [to_another_scope (textToIdentifier who) 
              [event_to_trigger event_following e_refuse_cond]
              ]
            )
          ]
        sequence $ map append_country_event [e', e_with_condition', e_without_condition, e_accept_cond, e_refuse_cond]
        return f'
  let f_final = foldl (\fm (who, condition) -> 
            do 
              f1 <- fm
              event_chain_of_country f1 who condition) (return f) l
  f_final

卖给美加 :: PrintEventState Focus
卖给美加 = 卖给_ [
  ("USA", [runParseDec "give_military_access = USA"]),
  ("CAN", [runParseDec "give_military_access = CAN"])
  ]

卖给国民法国和南非 :: PrintEventState Focus
卖给国民法国和南非 = 卖给_ [
  ("NFA", [runParseDec "add_popularity = { ideology = national_populist popularity = 0.05 }"]),
  ("SAF", [runParseDec "add_popularity = { ideology = national_populist popularity = 0.05 }"])
  ]

卖给第三国际 :: PrintEventState Focus
卖给第三国际 = 卖给_ [
        ("FRA", [
          runParseDec "add_popularity = { ideology = syndicalist popularity = 0.05 }",
          runParseDec "add_popularity = { ideology = radical_socialist popularity = 0.05 }"
          ])
      ]

卖给意大利和西班牙 :: PrintEventState Focus
卖给意大利和西班牙 = 卖给_ [
  ("ITA", [runParseDec "add_popularity = { ideology = national_populist popularity = 0.05 }"]),
  ("SPA", [runParseDec "add_popularity = { ideology = national_populist popularity = 0.05 }"])
  ]

开始财政紧缩 :: PrintEventState Focus
开始财政紧缩 = do
  f <- (newFocus $ focus_template {
      focus_id = "URG_start_austerity",
      focus_name = "开始财政紧缩",
      focus_desc = "开始财政紧缩",
      focus_cost = 2
      }) `add_focus_rewardM1` [runParseDec "add_political_power = 100"]
  (f', e) <- newEventFollowingFocus @CountryEvent event_following_immediate f 
                event_template {
                namespace = "URG",
                title = "开始财政紧缩",
                desc = "大规模抗议爆发",
                options = [
                  ("不好", [])
                ]
              }
  append_country_event e
  return f' 
final :: PrintEventState (Map FileName DocType)
final = do
  append_ideaM $ liftEventEnvToPrint $ 抛弃金本位制_idea
  append_scripted_ideaM $ liftEventEnvToPrint $ genBlackMondayDecrease 9
  sequence $ map append_focusM (map liftEventEnvToPrint [经济大萧条, 抛弃金本位制, 贱卖农产品] ++ [卖给美加, 卖给国民法国和南非, 卖给第三国际, 卖给意大利和西班牙, 开始财政紧缩])
  runPrinterWithLocalizationsM (const $ res_path `append` "Loc_test")
main :: IO ()
main = do
  uprint (runSingleParser parseDeclarations [r|
    URG_recover_from_economic_crisis = yes
		add_popularity = {
				ideology = authoritarian_democrat
				popularity = 0.1
		}
    |]
      )  
  let file_to_content = runPrintEnvEmpty final
  mapM_ (\(file, content) -> createAndWriteFile (unpack file) (ushow content)) $ toList file_to_content
