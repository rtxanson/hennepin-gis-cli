{-# LANGUAGE OverloadedStrings #-}

module Scrapegis 
    ( run
    ) where


import System.IO (stderr, hPutStrLn)

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

import Data.Csv ( toRecord
                , encode
                -- , encodeByName
                -- , Header
                )

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

import Data.List as L
-- import Data.Vector (fromList)

-- queryToCSV :: Maybe FeatureLookup -> B.ByteString
-- queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
-- queryToCSV Nothing = "" :: B.ByteString

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
  
    whenCmd "fetch" $ do

      whenCmd "city" $ do 
        let query_string = "MUNIC_CD = '01' (minneapolis)"
        hPutStrLn stderr $ "  Querying with: " ++ query_string

        doIt query_string

      whenCmd "zip" $ do
        zip_cd <- getOpt "<zip_code>"
        let query_string = "ZIP_CD ='" ++ zip_cd ++ "'" 
        hPutStrLn stderr $ "  Querying with: " ++ query_string
  
        doIt query_string
  
    whenCmd "query" $ do
      query_string <- getOpt "<query_string>"
  
      -- TODO: with optional object KML field
      -- TODO: option to specify chunk size. default, 900? 
      -- TODO: output as stuff becomes available-- don't need to store in mem.
      --
      doIt query_string

  where
      -- Processing funcs
      doQuery q = dataSource (T.pack q)
      cleanResult r = post_processing $ concatenateResults r

      -- Some docopt shortcuts
      whenCmd x = when $ opts `isPresent` (command x)
      whenOpt x =        opts `isPresent` (longOption x)
      getOpt  x =        opts `getArg` (argument x)

      -- Options
      dataSource = if whenOpt "mock"
                             then Mock.getHenCountyRecords
                             else Henn.getHenCountyRecords

      post_processing = if whenOpt "csv"
                            then featuresToCSV
                            else featuresToCSV

      -- configured process
      doIt q = do
          records <- doQuery q
          let recs = cleanResult records
          D8.putStrLn recs
