{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Scrapegis.Query where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import qualified Data.ByteString.Lazy.Char8 as D8
import Data.Text as T
import Data.List as L
import Control.Applicative
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

printTheThing :: Monad m => (D8.ByteString -> m b) -> OutputData -> m b
printTheThing f o = do printHeader >> printRecords
  where
    printHeader  = f $ csvHeader o
    printRecords = f $ featuresToCSV $ csvRecords o

handleOutput :: AppIO ()
handleOutput = do
  AppEnv { .. } <- ask
  AppState { .. } <- get

  let (Just rd) = resultData

  case outputFile of 
    "stdout" -> liftIO $ do 
      printTheThing (D8.putStrLn) rd
    otherwise -> liftIO $ do
      h <- openFile outputFile WriteMode
      printTheThing (D8.hPut h) rd
      hClose h
      hPutStrLn stderr $ "Written to: " ++ outputFile

-- configured process
runQuery :: AppIO ()
runQuery = do
  AppEnv { .. } <- ask
  AppState { .. } <- get

  liftIO $ do
     hPutStrLn stderr $ "  Querying with: " ++ queryString

  records <- Henn.getHenCountyRecords

  put $ AppState {
    resultData = Just OutputData {
         csvHeader = feature_header_bs
       , csvRecords = L.concat $ getFeatures <$> records
      }
  }

  handleOutput

