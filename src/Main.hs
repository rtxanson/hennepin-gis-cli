{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Scrapegis        (run)
-- import Scrapegis.Option (getOptions, getMode, usage)

import Scrapegis
import Scrapegis.Hennepin
-- import Scrapegis.MockHennepin
import Scrapegis.Types

import Data.Text

-- TODO: FeatureLookup -> CSV

import Data.Csv (toRecord, encode, Record)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

import Control.Applicative

query :: Text
query = "ZIP_CD = '55401'"

-- need a good cassava encoding example to run by
-- toCSV :: Maybe FeatureLookup -> B.ByteString
-- toCSV Nothing = []
-- toCSV (Just fl) = recs
--   where features = getFeatures fl
--         recs = encode $ toRecord <$> features

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ fmap toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

main = do
  records <- getHenCountyRecords query
  let recs = queryToCSV records
  D8.putStrLn recs

