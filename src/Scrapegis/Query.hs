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

import Scrapegis.Hennepin as Henn
-- import Scrapegis.MockHennepin as Mock
import Scrapegis.App
import Scrapegis.Types
import Scrapegis.Export

makeQueryString :: String -> String -> String
makeQueryString "zip"      aarg = "ZIP_CD = '" ++ aarg ++ "'"
makeQueryString "owner"    aarg = "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "taxpayer" aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "names"    aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "' OR " ++
                                  "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "city"     _ = "MUNIC_CD = '01'" -- (minneapolis)
makeQueryString "pid"      aarg = "PID = '" ++ aarg ++ "'"
makeQueryString _ _ = ""

-- configured process
runQuery :: AppIO ()
runQuery = do
  AppEnv { .. } <- ask
  AppState { .. } <- get

  liftIO $
     hPutStrLn stderr $ "  Querying with: " ++ queryString

  records <- Henn.getHenCountyRecords

  put $ AppState {
    resultData = Just OutputData {
         csvHeader = feature_header_bs
       , csvRecords = L.concat $ getFeatures <$> records
      }
  }

  handleOutput

