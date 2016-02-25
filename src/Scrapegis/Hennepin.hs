{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Hennepin
    ( getHenCountyRecords
    ) where

import Scrapegis.Types
import Scrapegis.Utils
import Scrapegis.Settings
import Scrapegis.Export

import Network.Wreq

import Control.Applicative
import Control.Lens

import Data.Aeson
import Data.Maybe (catMaybes)

import Data.Text as T
import Data.List as L

import qualified Data.ByteString.Lazy as B

import System.IO (stderr, hPutStrLn)

-- | This is the main access point to the Hennepin County GIS Property records.
-- | It initiates two requests: one to fetch object IDs, and the second which
-- | POSTs all the object IDs and returns actual objects. Requests falling under the 
-- | second are chunked by 900, so larger requests may take some time.

getHenCountyRecords :: Text -> IO [FeatureLookup]
getHenCountyRecords query_string = do
    m <- decodeIDResponseToJSON <$> idReq query_string
    bbq <- fetchInChunks m
    return $ parseLookups bbq
  where
    parseLookups :: [Response B.ByteString] -> [FeatureLookup]
    parseLookups ls = catMaybes $ decodeRecResponseToJSON <$> ls

-- | Generates a response object for a list of object IDs.

getRecordByIds :: [Integer] -> IO (Response B.ByteString)
getRecordByIds ids = post hennepin_gis_host args
  where
    args = [ "objectIds" := (ids_as_string :: T.Text)
           , "f" := ("json" :: T.Text)
           , "outFields" := ("*" :: T.Text)
           ] 

    -- IDs need to be one string joined with comma
    ids_as_string = T.intercalate comma id_strings
    id_strings = T.pack <$> [show i | i <- ids]
    comma = T.pack ", "

-- | For a given querystring, this returns a Response containing matching
-- | object IDs.

idReq :: Text -> IO (Response B.ByteString)
idReq querystring = getWith opts hennepin_gis_host
  where
    opts = defaults & param "where" .~ [querystring]
                    & param "f" .~ ["json"]
                    & param "returnIDsOnly" .~ ["true"]
                    & header "Accept" .~ ["application/json"]

-- | This runs two API queries that result in stuff: first gets the IDs, and then
-- | the remaining queries actually get the objects.

-- | For future learning: this could be easily replaced by mapM: `mapM getRecordByIDs chunks`.

fetchChunks :: [[Integer]] -> IO [Response B.ByteString]
fetchChunks [] = return []
fetchChunks (first:rest) = do
    hPutStrLn stderr $ status_message
    m <- getRecordByIds first -- :: Response B.ByteString
    mbs <- fetchChunks rest
    return $ m : mbs
  where
    remaining_count = L.length rest
    status_message
      | remaining_count > 0 = "Requests remaining: " ++ (show remaining_count)
      | otherwise           = "Requesting..."

fetchInChunks :: Maybe IDQueryResult -> IO [Response B.ByteString]
fetchInChunks ids = do
    let chunks = chunkArray 900 (getIDs ids)
    bbq <- fetchChunks chunks
    return $ bbq
  where
    getIDs :: Maybe IDQueryResult -> [Integer]
    getIDs (Just array) = getIDList array
    getIDs Nothing = []

decodeIDResponseToJSON :: Response B.ByteString -> Maybe IDQueryResult
decodeIDResponseToJSON r = (decode <$> r) ^. responseBody

decodeRecResponseToJSON :: Response B.ByteString -> Maybe FeatureLookup
decodeRecResponseToJSON r = (decode <$> r) ^. responseBody
