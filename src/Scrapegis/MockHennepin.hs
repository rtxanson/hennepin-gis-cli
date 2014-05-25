{-# LANGUAGE OverloadedStrings #-}

-- | This is just a module for producing mock data without querying actual
-- | endpoints. Aimed at faster developing.

module Scrapegis.MockHennepin
    ( getHenCountyRecords
    ) where

import Scrapegis.Types

import Control.Applicative

import qualified Data.ByteString.Lazy as B

import Data.Aeson

readJSONFile :: String -> IO B.ByteString
readJSONFile f_str = B.readFile filepath
  where
    filepath = f_str :: FilePath

readAndDecodeJSONFeatures :: String -> IO (Maybe FeatureLookup)
readAndDecodeJSONFeatures f_str = decode <$> (readJSONFile f_str)

readAndDecodeJSONIDResult :: String -> IO (Maybe IDQueryResult)
readAndDecodeJSONIDResult f_str = decode <$> (readJSONFile f_str)


getHenCountyRecords query_string = do
    m <- readAndDecodeJSONIDResult "test_data/object_ids.json"
    let ids = getIDs m

    mrecs <- readAndDecodeJSONFeatures "test_data/two_json.json"
    return mrecs
  where
    getIDs :: Maybe IDQueryResult -> [Integer]
    getIDs (Just array) = getIDList array
    getIDs Nothing = []


