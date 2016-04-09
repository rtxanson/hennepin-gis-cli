{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Scrapegis.Export (
   featuresToCSV
 , queryToCSV
 , handleOutput
) where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8 as D8

import Data.Csv ( toRecord
                , encode
                )

import Data.List as L
import qualified Data.ByteString.Lazy as B

import Scrapegis.App
import Scrapegis.Types

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

featuresToCSV :: [Feature] -> B.ByteString
featuresToCSV recs = encode $ L.map toRecord recs

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
