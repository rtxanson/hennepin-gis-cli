{-# LANGUAGE OverloadedStrings #-}

module Scrapegis 
    ( run
    ) where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import Scrapegis.Hennepin as Henn
import Scrapegis.MockHennepin as Mock
import Scrapegis.Types

-- Option parsing
import Control.Monad (when)

import System.Console.Docopt ( getArg
                             , getArgWithDefault
                             , isPresent
                             , command
                             , argument
                             , longOption
                             , Arguments
                             )

import Data.Text as T
import Data.List as L
-- import Data.Vector (fromList)

import Data.Csv ( toRecord
                , encode
                -- , encodeByName
                -- , Header
                )

import qualified Data.Aeson as AESON

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

-- TODO: Main.hs: FailedConnectionException2 "gis.co.hennepin.mn.us" 80 False
-- getAddrInfo: does not exist (nodename nor servname provided, or not known)
-- TODO: with optional object KML field

-- TODO: option to specify chunk size. default, 900? 
-- TODO: output as stuff becomes available-- don't need to store in mem.


-- TODO: output geojson polygons to kml snippets: https://hackage.haskell.org/package/geojson
-- http://hackage.haskell.org/package/gps-0.2.4/docs/Data-GPS.html
-- http://hackage.haskell.org/package/proj4-hs-bindings
-- https://github.com/pavpen/proj4-hs-bindings
--
-- http://hackage.haskell.org/package/txt-sushi
-- http://hackage.haskell.org/package/csv-conduit


-- http://hackage.haskell.org/package/esqueleto-1.4.1.2/docs/Database-Esqueleto.html

run :: Arguments -> IO ()
run opts = do

    whenCmd "fetch" $ do
        whenCmd "owner" $ do
            name_like <- getOpt "<name_like>"
            let query_string = "OWNER_NM LIKE '" ++ name_like ++ "'"
            hPutStrLn stderr $ "  Querying with: " ++ query_string
            doIt query_string

        whenCmd "taxpayer" $ do
            name_like <- getOpt "<name_like>"
            let query_string = "OWNER_NM LIKE '" ++ name_like ++ "'"
            hPutStrLn stderr $ "  Querying with: " ++ query_string
            doIt query_string

        whenCmd "names" $ do
            name_like <- getOpt "<name_like>"
            let query_string = "TAXPAYER_NM LIKE '" ++ name_like ++ "' OR " ++
                               "OWNER_NM LIKE '" ++ name_like ++ "'"
            hPutStrLn stderr $ "  Querying with: " ++ query_string
            doIt query_string

        whenCmd "city" $ do
            -- EDINA: 24
            -- ORONO: 38
            -- RICHFIELD: 42
            let query_string = "MUNIC_CD = '01'" -- (minneapolis)
            hPutStrLn stderr $ "  Querying with: " ++ query_string
            hPutStrLn stderr $ "                 (minneapolis)"

            doIt query_string

        whenCmd "zip" $ do
            zip_cd <- getOpt "<zip_code>"
            let query_string = "ZIP_CD = '" ++ zip_cd ++ "'"
            hPutStrLn stderr $ "  Querying with: " ++ query_string

            doIt query_string

        whenCmd "pid" $ do
            pid <- getOpt "<pid>"
            let query_string = "PID = '" ++ pid ++ "'"
            hPutStrLn stderr $ "  Querying with: " ++ query_string

            doIt query_string


    whenCmd "query" $ do
        query_string <- getOpt "<query_string>"

        doIt query_string

  where
      -- Processing funcs
      doQuery q = dataSource (T.pack q)
      cleanResult r = post_processing $ concatenateFeatures r

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
                            else if whenOpt "json"
                            	 then featuresToJSON
                            	 else featuresToCSV

      output_header = if whenOpt "no-header"
                            then False
                            else True

      output_file = getArgWithDefault opts "stdout" (longOption "out")

      -- configured process
      doIt q = do
          records <- doQuery q
          let recs = cleanResult records
          let header_str = D8.pack $ L.intercalate ("," :: String) feature_header_cols
          if output_file == "stdout"
              then do
                  when (output_header && whenOpt "csv") $ do D8.putStrLn header_str
                  D8.putStrLn recs
              else do
                  h <- openFile output_file WriteMode
                  when (output_header && whenOpt "csv") $ do D8.hPut h header_str
                  D8.hPut h recs
                  hClose h
                  hPutStrLn stderr $ "Written to: " ++ output_file

          hPutStrLn stderr $ "Done."

-- should shift these functions to Utils

-- import Data.Vector (fromList)

-- queryToCSV :: Maybe FeatureLookup -> B.ByteString
-- queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
-- queryToCSV Nothing = "" :: B.ByteString

-- TODO: header

-- queryToCSVWithHeader :: Maybe FeatureLookup -> B.ByteString
-- queryToCSVWithHeader (Just features) = encodeByName header records
--     where
--         -- This is what this is, but need: Header
--         header :: Data.Vector.Vector String
--         header = (fromList feature_header_cols)
-- 
--         records = L.map toRecord features
-- queryToCSVWithHeader Nothing = "" :: B.ByteString

featuresToCSV :: [Feature] -> B.ByteString
featuresToCSV recs = encode $ L.map toRecord recs

featuresToJSON :: [Feature] -> B.ByteString
featuresToJSON recs = AESON.encode recs

