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
getRecordByIds ids = getWith opts url
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
idReq querystring = getWith opts url
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

getChunk :: [[Integer]] -> IO [Maybe FeatureLookup]
getChunk [] = return []
getChunk (first:rest) = do
    recs <- getRecordByIds first
    let mrec = decodeRecResponseToJSON recs
    when (L.length rest > 0) $ return $ mrec : getChunk rest

-- interpretLines :: Map.Map String [Char] -> [[String]] -> IO ()
-- interpretLines cache [] = putStrLn ""
-- interpretLines cache chunks = do
--     let chunk:rest = chunks
--     (cache, sub_lines) <- lookupLines cache $ unlines chunk
--     let prtlines = unlines [a | a <- lines sub_lines, length a > 0]
--     putStrLn prtlines
--     when (length rest > 0) $ interpretLines cache rest


getHenCountyRecords' :: Text -> IO (Maybe FeatureLookup)
getHenCountyRecords' query_string = do
    r <- idReq query_string
    let m = decodeIDResponseToJSON r
    let ids = getIDs m
    let id_chunks = chunkArray 5 ids
    print id_chunks
    mrec <- getChunk id_chunks
    return mrec 
  where
    getIDs :: Maybe IDQueryResult -> [Integer]
    getIDs (Just array) = getIDList array
    getIDs Nothing = []

getHenCountyRecords :: Text -> IO (Maybe FeatureLookup)
getHenCountyRecords query_string = do
    r <- idReq query_string
    let m = decodeIDResponseToJSON r
    let ids = getIDs m
    let first_five =  L.take 5 ids
    print first_five
    print "querying first chunk"
    recs <- getRecordByIds first_five
    let mrec = decodeRecResponseToJSON recs
    return mrec
  where
    getIDs :: Maybe IDQueryResult -> [Integer]
    getIDs (Just array) = getIDList array
    getIDs Nothing = []
