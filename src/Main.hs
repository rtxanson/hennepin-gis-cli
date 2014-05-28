{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (stderr, hPutStrLn)

import Paths_scrapegis
import Scrapegis.Hennepin as Henn
import Scrapegis.MockHennepin as Mock
import Scrapegis.Types

-- Option parsing
import Control.Monad (when)

import System.Console.Docopt ( optionsWithUsageFile
                             , getArg
                             , isPresent
                             , command
                             , argument
                             , longOption
                             , Arguments
                             )

import Data.Text as T

import Data.Csv (toRecord, encode, encodeByName, Header)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

import Data.List as L
import Data.Vector (fromList)

query :: T.Text
query = "ZIP_CD = '55401'"

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

-- TODO: header
-- queryToCSVWithHeader :: Maybe FeatureLookup -> B.ByteString
-- queryToCSVWithHeader (Just recs) = encodeByName header records
--     where
--         header = (fromList feature_header_cols) :: Header
--         records = L.map toRecord (getFeatures recs)
-- queryToCSVWithHeader Nothing = "" :: B.ByteString

featuresToCSV :: [Feature] -> B.ByteString
featuresToCSV recs = encode $ L.map toRecord recs

justFeatures :: Maybe FeatureLookup -> [Feature]
justFeatures (Just f) = getFeatures f
justFeatures Nothing = []

concatenateResults :: [Maybe FeatureLookup] -> [Feature]
concatenateResults ms = fs
    where fs = L.concat (L.map justFeatures ms)

-- TODO: Main.hs: FailedConnectionException2 "gis.co.hennepin.mn.us" 80 False
-- getAddrInfo: does not exist (nodename nor servname provided, or not known)

run :: Arguments -> IO ()
run opts = do
    -- Begin option processing
    let post_processing = if whenOpt "csv"
                              then featuresToCSV
                              else featuresToCSV
  
    let dataSource = if whenOpt "mock"
                           then Mock.getHenCountyRecords
                           else Henn.getHenCountyRecords
  
    whenCmd "fetch" $ do
      let query_string = "MUNIC_CD = '01' (minneapolis)"
      hPutStrLn stderr $ "  Querying with: " ++ query_string
  
      records <- dataSource (T.pack query_string)
      let recs = post_processing $ concatenateResults records
      D8.putStrLn recs
  
    whenCmd "query" $ do
      query_string <- getOpt "<query_string>"
  
      -- TODO: with optional object KML field
      -- TODO: option to specify chunk size. default, 900? 
      -- TODO: output as stuff becomes available-- don't need to store in mem.
      --
      records <- dataSource (T.pack query_string)
      let recs = post_processing $ concatenateResults records
      D8.putStrLn recs

  where
      -- Some docopt shortcuts
      let whenCmd x = when (opts `isPresent` (command x))
      let whenOpt x = opts `isPresent` (longOption x)
      let getOpt  x = opts `getArg` (argument x)


main :: IO ()
main = do
  usage <- getDataFileName "src/Usage.txt"
  opts <- optionsWithUsageFile usage

  run opts

