{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Scrapegis        (run)
-- import Scrapegis.Option (getOptions, getMode, usage)

import Scrapegis
-- import Scrapegis.Hennepin
import Scrapegis.MockHennepin

import Scrapegis.Types

import Data.Text

-- TODO: FeatureLookup -> CSV

import Data.Csv (toRecord, encode)

import qualified Data.ByteString.Lazy as B

import Control.Applicative

query :: Text
query = "ZIP_CD = '55401'"

-- need a good cassava encoding example to run by
toCSV :: Maybe FeatureLookup -> B.ByteString
toCSV (Just fl) = recs
  where features = getFeatures fl
        recs = encode $ toRecord <$> features

toCSV Nothing = []

main = do
  records <- getHenCountyRecords query
  let features = getFeatures <$> records
  print $ liftA ((<$>) toRecord) features


