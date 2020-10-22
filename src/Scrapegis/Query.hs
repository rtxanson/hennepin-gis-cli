{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Scrapegis.Query where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import Data.List as L
import Control.Monad.Reader
import Control.Monad.State

import Scrapegis.Multnomah as Mult
-- import Scrapegis.MockHennepin as Mock
import Scrapegis.App
import Scrapegis.Types
import Scrapegis.Export

-- Field doc:
-- http://www3.multco.us/arcgispublic/rest/services/DART/Taxlots_Orion_Public/MapServer/layers
--
makeQueryString :: String -> String -> String
makeQueryString "zip"      aarg = "ZIP = '" ++ aarg ++ "'"
makeQueryString "owner"    aarg = "NAME LIKE '" ++ aarg ++ "'"
makeQueryString "taxpayer" aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "names"    aarg = "NAME LIKE '" ++ aarg ++ "' OR " ++
                                  "NAME2 LIKE '" ++ aarg ++ "'"
makeQueryString "city"     aarg = "CITY = '" ++ aarg ++ "'"
makeQueryString "pid"      aarg = "PROPID = '" ++ aarg ++ "'"
makeQueryString "propid"   aarg = "PROPID = '" ++ aarg ++ "'"
makeQueryString _ _ = ""

-- configured process
runQuery :: AppIO ()
runQuery = do
  AppEnv { .. } <- ask
  AppState { .. } <- get

  liftIO $
     hPutStrLn stderr $ "  Querying with: " ++ queryString

  records <- Mult.getHenCountyRecords

  put $ AppState {
    resultData = Just OutputData {
         csvHeader = feature_header_bs
       , csvRecords = L.concat $ getFeatures <$> records
      }
  }

  handleOutput

