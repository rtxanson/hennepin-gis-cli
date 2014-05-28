{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Scrapegis        (run)

import Scrapegis
import Scrapegis.Hennepin as Henn
import Scrapegis.MockHennepin as Mock
import Scrapegis.Types

-- Option parsing
import Control.Monad (when)
import Data.Char (toUpper)
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command, argument, longOption)

import Data.Text as T

import Data.Csv (toRecord, encode, Record)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

import Control.Applicative

import Data.List as L

query :: T.Text
query = "ZIP_CD = '55401'"

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

featuresToCSV :: [Feature] -> B.ByteString
featuresToCSV recs = encode $ L.map toRecord recs

justFeatures :: Maybe FeatureLookup -> [Feature]
justFeatures (Just f) = getFeatures f
justFeatures Nothing = []

concatenateResults :: [Maybe FeatureLookup] -> [Feature]
concatenateResults ms = fs
    where fs = L.concat (L.map justFeatures ms)


-- TODO: one more example of output
-- queryToJSON :: Maybe FeatureLookup -> B.ByteString
-- queryToJSON (Just recs) = encode $ fmap toRecord (getFeatures recs)
-- queryToJSON Nothing = "" :: B.ByteString

-- TODO: Main.hs: FailedConnectionException2 "gis.co.hennepin.mn.us" 80 False
-- getAddrInfo: does not exist (nodename nor servname provided, or not known)

main = do
  opts <- optionsWithUsageFile "Usage.txt"

  when (opts `isPresent` (command "query")) $ do
    query_string <- opts `getArg` (argument "<query_string>")

    let post_processing = if opts `isPresent` (longOption "csv")
                                  then featuresToCSV
                                  else featuresToCSV

    let dataSource = if opts `isPresent` (longOption "mock")
                           then Mock.getHenCountyRecords
                           else Henn.getHenCountyRecords

    -- TODO: chunk size option. default, 900? 
    -- TODO: output as stuff becomes available-- don't need to store in mem.
    --
    records <- dataSource (T.pack query_string)
    let recs = post_processing $ concatenateResults records
    D8.putStrLn recs

