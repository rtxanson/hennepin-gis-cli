{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Hennepin
    ( getHenCountyRecords
    ) where

import Scrapegis.Types
import Scrapegis.Utils

import Network.Wreq

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.List as L
import Data.Text as T
import Data.Map as M

import qualified Data.ByteString.Lazy as B

import Data.Aeson

decodeIDResponseToJSON :: Response B.ByteString -> Maybe IDQueryResult
decodeIDResponseToJSON r = (decode <$> r) ^. responseBody

decodeRecResponseToJSON :: Response B.ByteString -> Maybe FeatureLookup
decodeRecResponseToJSON r = (decode <$> r) ^. responseBody

hengishost =  "http://gis.co.hennepin.mn.us/ArcGIS/rest/services/Maps/PROPERTY/MapServer/0/query"

getRecordByIds :: [Integer] -> IO (Response B.ByteString)
getRecordByIds ids = postWith opts url (partText "")
  where
    opts = defaults & param "objectIds" .~ [ids_as_string]
                    & param "f" .~ ["json"]
                    & param "outFields" .~ ["*"]
                    & header "Accept" .~ ["application/json"]
    url = hengishost

    -- IDs need to be one string joined with comma
    ids_as_string = T.intercalate comma id_strings
    id_strings = T.pack <$> [show i | i <- ids]
    comma = T.pack ", "

idReq :: Text -> IO (Response B.ByteString)
idReq querystring = postWith opts url (partText "")
  where
    opts = defaults & param "where" .~ [querystring]
                    & param "f" .~ ["json"]
                    & param "returnIDsOnly" .~ ["true"]
                    & header "Accept" .~ ["application/json"]
    url = hengishost

-- | This runs two queries that result in stuff: first to get the IDs, and then the remaining queries actually get the objects.
--
-- | TODO: finish up the chunking part. For now this is good enough for
-- | testing.

-- TODO: note that this can basically be replaced by mapM: mapM getRecordByIDs chunks
fetchChunks :: [[Integer]] -> IO [Maybe FeatureLookup]
fetchChunks [] = return []
fetchChunks (first:rest) = do
   -- TODO: stderr here, also list chunk x of total
    print "Fetching chunk"
    m <- getRecordByIds first -- :: Response B.ByteString
    let rs = decodeRecResponseToJSON m
    mbs <- fetchChunks rest
    return $ rs : mbs

getHenCountyRecords :: Text -> IO [Maybe FeatureLookup]
getHenCountyRecords query_string = do
    m <- decodeIDResponseToJSON <$> idReq query_string
    let chunks = chunkArray 100 (getIDs m)
    bbq <- fetchChunks chunks
    return bbq

  where

    getIDs :: Maybe IDQueryResult -> [Integer]
    getIDs (Just array) = getIDList array
    getIDs Nothing = []


