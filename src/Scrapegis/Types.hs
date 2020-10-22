{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards  #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature(..)
    , OutputData(..)
    , RequestBatch(..)
    , FeatureLookup(..)
    , FeatureAttributes(..)
    , getIDList
    , feature_header_bs
    ) where

import Data.List as L
import Data.Text as T

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as D8

import Data.Csv (toRecord, toField, ToRecord, record)

import GHC.Generics

data OutputData = OutputData 
  { csvHeader  :: D8.ByteString
  , csvRecords :: [Feature]
  } deriving (Show)

data RequestBatch = RequestBatch 
  { batchNumber :: Int
  , batchTotal :: Int
  , batchValues :: [Integer]
  } deriving (Show)

-- | Each JSON result contains a list of `Feature` objects, which contains both
-- | attributes and geographical information. FeatureAttributes handles only
-- | the attribute section.

data FeatureAttributes = FeatureAttributes 
    { getOBJECTID_1     :: Integer
    -- , getSHAPE          :: esriFieldTypeGeometry
    , getMAPTAXLOT      :: String
    , getPROPID         :: String
    , getALTACCTNUM     :: String
    , getNAME           :: String
    , getNAME2          :: String
    , getADDR1          :: String
    , getADDR2          :: String
    , getCITY           :: String
    , getSTATE          :: String
    , getZIP            :: String
    , getSITUSNUM       :: String
    , getSITUSDIR       :: String
    , getSITUSNAME      :: String
    , getSITUSSUFFIX    :: String
    , getSITUSSUFFIX2   :: String
    , getSITUSUNITTYPE  :: String
    , getSITUSUNITNUM   :: String
    , getSITUSADDR      :: String
    , getSITUSCITY      :: String
    , getSITUSSTATE     :: String
    , getSITUSZIP       :: String
    , getMAPID          :: String
    , getLEGAL          :: String
    , getTRACTLOT       :: String
    , getBLOCK          :: String
    , getADDLEGAL       :: String
    , getLOC_CODE       :: String
    , getACCOUNT_STATUS :: String
    , getLEVYCODE       :: String
    , getNBOCODE        :: String
    , getIMP_COUNT      :: String
    , getPROPCLASS      :: String
    , getPROP_CODE      :: String
    , getDEED_TYPE      :: String
    , getINST_NUM       :: String
    , getDEED_DATE      :: String -- esriFieldTypeDate
    , getSALE_PRICE     :: Double
    , getSALE_DATE      :: String -- esriFieldTypeDate
    , getEXEMPTION      :: String
    , getZONING         :: String
    , getSIZEACRES      :: Double
    , getSIZESQFT       :: Integer
    , getIMPTYPE        :: String
    , getACTYEARBUILT   :: Integer
    , getMAINAREA       :: Integer
    , getUNITS          :: Integer
    , getMAIN_SQFT      :: Integer
    , getROLLYEAR       :: Integer -- SmallInteger
    , getROLLLAND       :: Double
    , getROLLIMP        :: Double
    , getROLLM50        :: Double
    , getSHAPE_Length   :: Double
    , getSHAPE_Area     :: Double
    , getxCentroid      :: Double
    , getyCentroid      :: Double
    , getPROPERTY_TYPE  :: String
    , getAPPRAISER      :: String
    , getROLLMAV        :: Double
    } deriving (Generic, Show)

featureOpts :: Options
featureOpts = defaultOptions { fieldLabelModifier = L.drop 3 }

-- | Control parsing the JSON into a Haskell data type.

instance ToJSON FeatureAttributes where
  toEncoding = genericToEncoding featureOpts
  toJSON     = genericToJSON featureOpts

instance FromJSON FeatureAttributes where
  parseJSON = genericParseJSON featureOpts

-- | This handles the JSON returned by the first object ID query. It returns no
-- | data apart from the object IDs, which are later chunked and processed
-- | FeatureLookups.

data IDQueryResult = IDQueryResult { getIDList :: [Integer]
                                   } deriving (Show)

-- | JSON parsing for IDQueryResult

instance FromJSON IDQueryResult where
  parseJSON (Object o) = IDQueryResult <$> (o .: "objectIds")
  parseJSON _ = fail "IDQueryResult could not be parsed"

-- | This is returned in FeatureLookups queries.

data Feature = Feature { featureAttributes :: FeatureAttributes
                       } deriving (Show)

-- | JSON parsing for Feature objects. Simple for now.

instance FromJSON Feature where
  parseJSON (Object o) = Feature <$> o .: "attributes"
  parseJSON _ = fail "Feature could not be parsed"

-- | This is the result of querying a set of IDs.

data FeatureLookup = FeatureLookup { getFeatures :: [Feature]
                                   , displayFieldName :: String
                                   -- , getFieldAliases :: M.Map String String
                                   -- , getSpatialReference :: M.Map String Integer
                                   -- , getGeometryType :: String
                                   } deriving (Show)

-- | FeatureLookup JSON parsing.

instance FromJSON FeatureLookup where
  parseJSON (Object o) = FeatureLookup <$> o .: "features"
                                       <*> o .: "displayFieldName"
                                       -- <*> o .: "fieldAliases"
                                       -- <*> o .: "spatialReference"
                                       -- <*> o .: "geometryType"
  parseJSON _ = fail "FeatureLookup could not be parsed"


-- | FeatureLookup CSV serialization.

instance ToRecord FeatureLookup where
  toRecord feat = record [toField fieldname, toField fieldname]
    where
      fieldname = displayFieldName feat

feature_header_bs :: D8.ByteString
feature_header_bs = heddr
  where
    heddr = D8.pack $ (commad ++ "\n")
    commad = L.intercalate ("," :: String) feature_header_cols

feature_header_cols :: [String]
feature_header_cols = [ "OBJECTID_1"
                      -- , "SHAPE"
                      , "MAPTAXLOT"
                      , "PROPID"
                      , "ALTACCTNUM"
                      , "NAME"
                      , "NAME2"
                      , "ADDR1"
                      , "ADDR2"
                      , "CITY"
                      , "STATE"
                      , "ZIP"
                      , "SITUSNUM"
                      , "SITUSDIR"
                      , "SITUSNAME"
                      , "SITUSSUFFIX"
                      , "SITUSSUFFIX2"
                      , "SITUSUNITTYPE"
                      , "SITUSUNITNUM"
                      , "SITUSADDR"
                      , "SITUSCITY"
                      , "SITUSSTATE"
                      , "SITUSZIP"
                      , "MAPID"
                      , "LEGAL"
                      , "TRACTLOT"
                      , "BLOCK"
                      , "ADDLEGAL"
                      , "LOC_CODE"
                      , "ACCOUNT_STATUS"
                      , "LEVYCODE"
                      , "NBOCODE"
                      , "IMP_COUNT"
                      , "PROPCLASS"
                      , "PROP_CODE"
                      , "DEED_TYPE"
                      , "INST_NUM"
                      , "DEED_DATE"
                      , "SALE_PRICE"
                      , "SALE_DATE"
                      , "EXEMPTION"
                      , "ZONING"
                      , "SIZEACRES"
                      , "SIZESQFT"
                      , "IMPTYPE"
                      , "ACTYEARBUILT"
                      , "MAINAREA"
                      , "UNITS"
                      , "MAIN_SQFT"
                      , "ROLLYEAR"
                      , "ROLLLAND"
                      , "ROLLIMP"
                      , "ROLLM50"
                      , "SHAPE_Length"
                      , "SHAPE_Area"
                      , "xCentroid"
                      , "yCentroid"
                      , "PROPERTY_TYPE"
                      , "APPRAISER"
                      , "ROLLMAV"
                      ]

-- cleanText (String s) = String $ T.strip s

instance ToRecord Feature where
  toRecord feat = record row_fields
    where
      clean = T.unpack . T.strip . T.pack
      cleanDate x = m ++ "/" ++ y
        where m = L.drop 4 x
              y = L.take 4 x

      field_accessors = [ (show . getOBJECTID_1)
                        -- , getSHAPE :: esriFieldTypeGeometry
                        , getMAPTAXLOT 
                        , getPROPID 
                        , getALTACCTNUM 
                        , getNAME 
                        , getNAME2 
                        , getADDR1 
                        , getADDR2 
                        , getCITY 
                        , getSTATE 
                        , getZIP 
                        , getSITUSNUM 
                        , getSITUSDIR 
                        , getSITUSNAME 
                        , getSITUSSUFFIX 
                        , getSITUSSUFFIX2 
                        , getSITUSUNITTYPE 
                        , getSITUSUNITNUM 
                        , getSITUSADDR 
                        , getSITUSCITY 
                        , getSITUSSTATE 
                        , getSITUSZIP 
                        , getMAPID 
                        , getLEGAL 
                        , getTRACTLOT 
                        , getBLOCK 
                        , getADDLEGAL 
                        , getLOC_CODE 
                        , getACCOUNT_STATUS
                        , getLEVYCODE 
                        , getNBOCODE 
                        , getIMP_COUNT 
                        , getPROPCLASS 
                        , getPROP_CODE 
                        , getDEED_TYPE 
                        , getINST_NUM 
                        , getDEED_DATE -- esriFieldTypeDate
                        , (show . getSALE_PRICE)
                        , getSALE_DATE -- esriFieldTypeDate
                        , getEXEMPTION 
                        , getZONING 
                        , (show . getSIZEACRES)
                        , (show . getSIZESQFT)
                        , getIMPTYPE 
                        , (show . getACTYEARBUILT)
                        , (show . getMAINAREA)
                        , (show . getUNITS)
                        , (show . getMAIN_SQFT)
                        , (show . getROLLYEAR) -- SmallInteger
                        , (show . getROLLLAND)
                        , (show . getROLLIMP)
                        , (show . getROLLM50)
                        , (show . getSHAPE_Length)
                        , (show . getSHAPE_Area)
                        , (show . getxCentroid)
                        , (show . getyCentroid)
                        , getPROPERTY_TYPE 
                        , getAPPRAISER 
                        , (show . getROLLMAV)
                        ]

      row_fields = toField <$> [clean (f attrs) | f <- field_accessors]
      attrs = featureAttributes feat

