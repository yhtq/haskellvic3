{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module Utilities where
import ParadoxGeneratorEnv
import BaseParser
import Parser
import BasePrinter
import Data.Text(Text, append, cons, pack, unpack)
import Data.List(uncons)
import Control.Monad(liftM)

res_path :: FileName
res_path = "./test_output/"
focus_file_name :: FileName
focus_file_name = res_path `append` "focuses_test"
country_event_file_name :: FileName
country_event_file_name = res_path `append` "country_events_test"
news_event_file_name :: FileName
news_event_file_name = res_path `append` "news_events_test"
idea_file_name :: FileName
idea_file_name = res_path `append` "ideas_test"
append_focus :: (MonadPrintEnv m, ParadoxPrintable a) => a -> m ()
append_focus = append_in_file focus_file_name
append_country_event :: (MonadPrintEnv m, ParadoxPrintable a) => a -> m ()
append_country_event = append_in_file country_event_file_name
append_news_event :: (MonadPrintEnv m, ParadoxPrintable a) => a -> m ()
append_news_event = append_in_file news_event_file_name
append_idea :: (MonadPrintEnv m, ParadoxPrintable a) => a -> m ()
append_idea = append_in_file idea_file_name
append_scripted_idea :: (MonadPrintEnv m, ParadoxPrintable a) => a -> m ()
append_scripted_idea = append_in_file $ res_path `append` "scripted_ideas_test"
append_focusM :: (MonadPrintEnv m, ParadoxPrintable a) => m a -> m ()
append_focusM = (=<<) append_focus
append_country_eventM :: (MonadPrintEnv m, ParadoxPrintable a) => m a -> m ()
append_country_eventM = (=<<) append_country_event
append_news_eventM :: (MonadPrintEnv m, ParadoxPrintable a) => m a -> m ()
append_news_eventM = (=<<) append_news_event
append_ideaM :: (MonadPrintEnv m, ParadoxPrintable a) => m a -> m ()
append_ideaM = (=<<) append_idea
append_scripted_ideaM :: (MonadPrintEnv m, ParadoxPrintable a) => m a -> m ()
append_scripted_ideaM = (=<<) append_scripted_idea



alwaysDec :: ValueBool -> Dec
alwaysDec v = ("always", toExp v)
intToText :: Int -> Text
intToText = pack . show
-- 按照给定 list 的顺序从前向后依次变化
genSeqOfIdeaChange :: [Text] ->  [Dec]
genSeqOfIdeaChange [] = error "至少两个 name"
genSeqOfIdeaChange (names@(name : rest_names)) = if length names <= 1 then error "至少两个 name" else 
  let namesWithFollowing = zip names rest_names in
    if uncons namesWithFollowing == Nothing then error "至少两个 name" else
  let Just (_first, _rest) = uncons namesWithFollowing in
    [
      (
        "if" :: Identifier,
          swap_to_next _first
      )
    ] ++ map (\i -> 
      (
        "else_if", 
        swap_to_next i
      )
    ) _rest ++ [
      (
        "else",
        toExp $ ("remove_ideas" :: Identifier, untypedIDToExp $ last rest_names)
      )
    ]  
    where swap_to_next (pres_name, next_name) = toExp [
              (
                "limit" :: Identifier, 
                toExp ("has_idea" :: Identifier, untypedIDToExp $ pres_name)
              ),
              (
                "swap_ideas", 
                  toExp [
                      ("remove_idea" :: Identifier, untypedIDToExp $ pres_name),
                      ("add_idea" :: Identifier, untypedIDToExp $ next_name)
                    ]
              )
            ]
comment_dec :: Text -> Dec
comment_dec = ("#",) . toExp
add_focus_reward :: [Dec] -> Focus -> Focus
add_focus_reward rewards focus = addsInField focus "completion_reward" rewards
add_focus_rewardM :: Monad m => [Dec] -> m Focus -> m Focus
add_focus_rewardM rewards = liftM (add_focus_reward rewards)
add_focus_rewardM1 :: Monad m =>  m Focus -> [Dec]  -> m Focus
add_focus_rewardM1 = flip add_focus_rewardM
add_timed_idea_on_focus :: Identifier -> ValueInt -> Focus -> Focus
add_timed_idea_on_focus idea_name days focus = add_focus_reward 
  [
    ("add_timed_idea" :: Identifier, toExp [
      ("idea" :: Identifier , toExp idea_name),
      ("days", toExp days)
    ])
    ]
    focus
add_timed_idea_on_focusM :: Monad m => Identifier -> ValueInt -> m Focus -> m Focus
add_timed_idea_on_focusM idea_name days = liftM (add_timed_idea_on_focus idea_name days)
add_timed_idea_on_focusM1 :: Monad m => m Focus -> Identifier -> ValueInt ->  m Focus
add_timed_idea_on_focusM1 focus id time =  add_timed_idea_on_focusM id time focus
add_custom_effect_tooltip_on_focus :: Identifier -> Focus ->  Focus
add_custom_effect_tooltip_on_focus tooltip focus = add_focus_reward 
  [
    ("custom_effect_tooltip" :: Identifier, toExp tooltip)
    ]
  focus
add_custom_effect_tooltip_on_focusM :: Monad m => Identifier -> m Focus -> m Focus
add_custom_effect_tooltip_on_focusM  tooltip = liftM (add_custom_effect_tooltip_on_focus tooltip) 
add_custom_effect_tooltip_on_focusM1 :: Monad m => m Focus -> Identifier ->  m Focus
add_custom_effect_tooltip_on_focusM1 = flip add_custom_effect_tooltip_on_focusM

to_another_scope :: Scope -> [Dec] -> Dec
to_another_scope scope decs = (
    scope,
    toExp $ unnameedObject decs
  )

runParseDecs :: TokenType -> [Dec]
runParseDecs = runSingleParser parseDeclarations
runParseDec :: TokenType -> Dec
runParseDec t = case uncons $ runParseDecs t of
  Just (d, []) -> d
  _ -> error $ unpack $ "runParseDec parse error: " `append` t