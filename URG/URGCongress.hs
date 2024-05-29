{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module URGCongress where
import Utilities
import ParadoxGeneratorEnv
import BaseParser
import Parser
import Text.RawString.QQ
import Data.Text (Text, append, concat, cons, pack, singleton, unpack, intercalate)
import Data.Map (toList, Map)
import Data.List(uncons)
import Prettyprinter(pretty)
representatives = "representatives" :: Text
senators = "senators" :: Text

-- 红党
colorado = "Colorado" :: Text

-- 埃雷拉派
herrera = "Herrera" :: Text
-- 里维拉派
rios = "Rios" :: Text
-- 特拉派
terra = "Terra" :: Text
colorado_faction = [herrera, rios, terra] :: [Text]


-- 民族党
nacional = "Nacional" :: Text

-- 巴特列派
batlle = "Batlle" :: Text
-- 萨拉维亚派
saravia = "Saravia" :: Text
nacional_faction = [batlle, saravia] :: [Text]


