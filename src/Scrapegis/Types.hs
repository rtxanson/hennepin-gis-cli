{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature(..)
    , RunParams(..)
    , FeatureLookup(..)
    , FeatureAttributes(..)
    , justFeatures
    , concatenateFeatures
    , getIDList
    , feature_header_cols
    ) where

-- import Data.Text as T
import Data.List as L
-- import Data.Map as M

import Data.Aeson
import Data.Aeson.Types

import Control.Applicative
import Data.Csv (toRecord, toField, ToRecord, record)

import GHC.Generics

import Control.Monad (mzero)

-- | Parameters for the main run.

data RunParams = RunParams {
    outputFile :: FilePath
  , queryString :: String
  } deriving (Show)


-- | Each JSON result contains a list of `Feature` objects, which contains both
-- | attributes and geographical information. FeatureAttributes handles only
-- | the attribute section.

data FeatureAttributes = FeatureAttributes {
      getPID :: String
    , getHOUSE_NO :: Integer
    , getSTREET_NM :: String
    , getZIP_CD :: String
    , getOWNER_NM :: String
    , getTAXPAYER_NM :: String
    , getTAXPAYER_NM_1 :: String
    , getTAXPAYER_NM_2 :: String
    , getTAXPAYER_NM_3 :: String
    , getABBREV_ADDN_NM :: String
    , getBLOCK :: String
    , getLOT :: String
    -- , getHMSTD_CD1 :: String
    -- , getHMSTD_CD1_NM :: String
    -- , getADDITION_NO :: String
    -- , getMKT_VAL_TOT :: Integer
    -- , getTAX_TOT :: Integer
    -- , getFORFEIT_LAND_IND :: String
    -- , getBUILD_YR :: String
    -- , getMUNIC_CD :: String
    -- , getMUNIC_NM :: String


    -- TODO: rename
    -- , getAbstrTorrensCode :: String
    -- , getTorrensType :: String
    -- , getCondoNumber :: String
    -- , getContigInd1 :: String
    -- , getCoopInd :: String
    -- , getNetTaxCapacity :: Integer
    -- , getEstBldgMktVal1 :: Integer
    -- , getEstBldgMktVal2 :: Integer
    -- , getEstBldgMktVal3 :: Integer
    -- , getEstBldgMktVal4 :: Integer
    -- , getEstLandMktVal1 :: Integer
    -- , getEstLandMktVal2 :: Integer
    -- , getEstLandMktVal3 :: Integer
    -- , getEstLandMktVal4 :: Integer
    -- , getFeatureCode :: Integer
    -- , getFracHouseNumber :: String
    -- , getMailingMunicCode :: String
    -- , getMailingMunicName :: String
    -- , getMetesBnds1 :: String
    -- , getMetesBnds2 :: String
    -- , getMetesBnds3 :: String
    -- , getMetesBnds4 :: String
    -- , getMoreMetesBndsInd :: String
    -- , getMultiAddrInd :: String
    -- , getObjectId :: Integer
    -- TODO: what type is this actually? , getParcelArea :: String

    -- TODO: rename
    -- , getPidText :: String
    -- , getPropertyStatusCd :: String
    -- , getPropertyTypeCd1 :: String
    -- , getPropertyTypeCd1Name :: String
    -- , getPropertyTypeCd2 :: String
    -- , getPropertyTypeCd3 :: String
    -- , getPropertyTypeCd4 :: String
    -- , getSaleCode :: String
    -- , getSaleCodeName :: String
    -- , getSaleDate :: String
    -- , getSalePrice :: Integer
    -- , getSchoolDistNo :: String
    -- , getSewerDistNo :: String
    -- , getStateCd :: Integer
    -- TODO: expecting this needs another type - , getShapeArea :: String
    -- TODO: expecting this needs another type - , getShapeLen :: String

    -- TODO: rename
    -- , getTifProjectNumber :: String
    -- , getWatershedNumber :: String
    } deriving (Generic, Show)

-- | Control parsing the JSON into a Haskell data type.

instance ToJSON FeatureAttributes where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (L.drop 3) }
  toJSON     = genericToJSON defaultOptions { fieldLabelModifier = (L.drop 3) }


instance FromJSON FeatureAttributes where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (L.drop 3) }

  -- TODO: keep this around as notes on postprocessing

  -- parseJSON (Object o) = 
  --   FeatureAttributes <$> (o .: "PID")
  --                     <*> (o .: "HOUSE_NO")
  --                     <*> clean "STREET_NM"
  --                     <*> (o .: "ZIP_CD")
  --                     <*> clean "OWNER_NM"
  --                     <*> clean "TAXPAYER_NM"
  --                     <*> clean "TAXPAYER_NM_1"
  --                     <*> clean "TAXPAYER_NM_2"
  --                     <*> clean "TAXPAYER_NM_3"
  --                     <*> clean "ABBREV_ADDN_NM"
  --                     <*> clean "BLOCK"
  --                     <*> clean "LOT"
  --                     <*> (o .: "HMSTD_CD1")
  --                     <*> clean "HMSTD_CD1_NAME"
  --                     <*> clean "ADDITION_NO"
  --                     <*> (o .: "MKT_VAL_TOT")
  --                     <*> (o .: "TAX_TOT")
  --                     <*> clean "FORFEIT_LAND_IND"
  --                     <*> (o .: "BUILD_YR")
  --                     <*> (o .: "MUNIC_CD")
  --                     <*> clean "MUNIC_NM"
  --                     <*> clean "ABSTR_TORRENS_CD"
  --                     <*> clean "TORRENS_TYP"
  --                     <*> clean "CONDO_NO"
  --                     <*> clean "CONTIG_IND1"
  --                     <*> clean "CO_OP_IND"
  --                     <*> (o .: "NET_TAX_CAPACITY")
  --                     <*> (o .: "EST_BLDG_MKT_VAL1")
  --                     <*> (o .: "EST_BLDG_MKT_VAL2")
  --                     <*> (o .: "EST_BLDG_MKT_VAL3")
  --                     <*> (o .: "EST_BLDG_MKT_VAL4")
  --                     <*> (o .: "EST_LAND_MKT_VAL1")
  --                     <*> (o .: "EST_LAND_MKT_VAL2")
  --                     <*> (o .: "EST_LAND_MKT_VAL3")
  --                     <*> (o .: "EST_LAND_MKT_VAL4")
  --                     <*> (o .: "FEATURECODE")
  --                     <*> clean "FRAC_HOUSE_NO"
  --                     <*> clean "MAILING_MUNIC_CD"
  --                     <*> clean "MAILING_MUNIC_NM"
  --                     <*> clean "METES_BNDS1"
  --                     <*> clean "METES_BNDS2"
  --                     <*> clean "METES_BNDS3"
  --                     <*> clean "METES_BNDS4"
  --                     <*> clean "MORE_METES_BNDS_IND"
  --                     <*> clean "MULTI_ADDR_IND"
  --                     <*> (o .: "OBJECTID")
  --                     -- <*> clean "PARCEL_AREA"
  --                     <*> clean "PID_TEXT"
  --                     <*> clean "PROPERTY_STATUS_CD"
  --                     <*> clean "PROPERTY_TYPE_CD1"
  --                     <*> clean "PROPERTY_TYPE_CD1_NAME"
  --                     <*> clean "PROPERTY_TYPE_CD2"
  --                     <*> clean "PROPERTY_TYPE_CD3"
  --                     <*> clean "PROPERTY_TYPE_CD4"
  --                     <*> clean "SALE_CODE"
  --                     <*> clean "SALE_CODE_NAME"
  --                     <*> (o .: "SALE_DATE")
  --                     <*> (o .: "SALE_PRICE")
  --                     <*> clean "SCHOOL_DIST_NO"
  --                     <*> clean "SEWER_DIST_NO"
  --                     <*> (o .: "STATE_CD")
  --                     -- <*> (o .: "Shape.area")
  --                     -- <*> (o .: "Shape.len")
  --                     <*> clean "TIF_PROJECT_NO"
  --                     <*> (o .: "WATERSHED_NO")
  --   where
  --     -- needs special handling because there's tons of crap whitespace
  --     clean x = T.strip <$> o .: x

  -- parseJSON  _ = mzero

-- | This handles the JSON returned by the first object ID query. It returns no
-- | data apart from the object IDs, which are later chunked and processed
-- | FeatureLookups.

data IDQueryResult = IDQueryResult { getIDList :: [Integer]
                                   } deriving (Show)

-- | JSON parsing for IDQueryResult

instance FromJSON IDQueryResult where
  parseJSON (Object o) = IDQueryResult <$> (o .: "objectIds")
  parseJSON  _ = mzero

-- | This is returned in FeatureLookups queries.

data Feature = Feature { featureAttributes :: FeatureAttributes
                       } deriving (Show)

-- | JSON parsing for Feature objects. Simple for now.

instance FromJSON Feature where
  parseJSON (Object o) = Feature <$> o .: "attributes"
  parseJSON  _ = mzero

-- | Return a list of features or nothing depending on the result of the query.

justFeatures :: Maybe FeatureLookup -> [Feature]
justFeatures (Just f) = getFeatures f
justFeatures Nothing = []

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
  parseJSON  _ = mzero


-- | FeatureLookup CSV serialization.

instance ToRecord FeatureLookup where
  toRecord feat = record [toField fieldname, toField fieldname]
    where
      fieldname = displayFieldName feat

concatenateFeatures :: [Maybe FeatureLookup] -> [Feature]
concatenateFeatures ms = fs
    where fs = L.concat (L.map justFeatures ms)

feature_header_cols :: [String]
feature_header_cols = [ "PID"
                      , "HOUSE_NO"
                      , "STREET_NM"
                      , "ZIP_CD"
                      -- , "OWNER_NM"
                      -- , "TAXPAYER_NM"
                      -- , "TAXPAYER_NM_1"
                      -- , "TAXPAYER_NM_2"
                      -- , "TAXPAYER_NM_3"
                      -- , "ABBREV_ADDN_NM"
                      -- , "BLOCK"
                      -- , "LOT"
                      -- , "HMSTD_CD1"
                      -- , "HMSTD_CD1_NAME"
                      -- , "ADDITION_NO"
                      -- , "MKT_VAL_TOT"
                      -- , "TAX_TOT"
                      -- , "FORFEIT_LAND_IND"
                      -- , "BUILD_YR"
                      -- , "MUNIC_CD"
                      -- , "MUNIC_NM"
                      -- , "ABSTR_TORRENS_CD"
                      -- , "TORRENS_TYP"
                      -- , "CONDO_NO"
                      -- , "CONTIG_IND1"
                      -- , "CO_OP_IND"
                      -- , "NET_TAX_CAPACITY"
                      -- , "EST_BLDG_MKT_VAL1"
                      -- , "EST_BLDG_MKT_VAL2"
                      -- , "EST_BLDG_MKT_VAL3"
                      -- , "EST_BLDG_MKT_VAL4"
                      -- , "EST_LAND_MKT_VAL1"
                      -- , "EST_LAND_MKT_VAL2"
                      -- , "EST_LAND_MKT_VAL3"
                      -- , "EST_LAND_MKT_VAL4"
                      -- , "FEATURECODE"
                      -- , "FRAC_HOUSE_NO"
                      -- , "MAILING_MUNIC_CD"
                      -- , "MAILING_MUNIC_NM"
                      -- , "METES_BNDS1"
                      -- , "METES_BNDS2"
                      -- , "METES_BNDS3"
                      -- , "METES_BNDS4"
                      -- , "MORE_METES_BNDS_IND"
                      -- , "MULTI_ADDR_IND"
                      -- , "OBJECTID"
                      -- -- <*> clean "PARCEL_AREA"
                      -- , "PID_TEXT"
                      -- , "PROPERTY_STATUS_CD"
                      -- , "PROPERTY_TYPE_CD1"
                      -- , "PROPERTY_TYPE_CD1_NAME"
                      -- , "PROPERTY_TYPE_CD2"
                      -- , "PROPERTY_TYPE_CD3"
                      -- , "PROPERTY_TYPE_CD4"
                      -- , "SALE_CODE"
                      -- , "SALE_CODE_NAME"
                      -- , "SALE_DATE"
                      -- , "SALE_PRICE"
                      -- , "SCHOOL_DIST_NO"
                      -- , "SEWER_DIST_NO"
                      -- , "STATE_CD"
                      -- -- <*> (o .: "Shape.area")
                      -- -- <*> (o .: "Shape.len")
                      -- , "TIF_PROJECT_NO"
                      -- , "WATERSHED_NO"
                      ]

instance ToRecord Feature where
  toRecord feat = record row_fields
    where
      -- TODO:
      --   there's more data cleaning to be done, probably want to combine
      --   taxpayer names, and address and such

      -- new fields go here
      field_accessors = [ getPID
                        , (show . getHOUSE_NO )
                        , getSTREET_NM
                        , getZIP_CD
                        , getOWNER_NM
                        , getTAXPAYER_NM
                        , getTAXPAYER_NM_1
                        , getTAXPAYER_NM_2
                        , getTAXPAYER_NM_3
                        , getABBREV_ADDN_NM
                        , getBLOCK
                        , getLOT
                        -- , getHMSTD_CD1
                        -- , getHMSTD_CD1_NM
                        -- , getADDITION_NO
                        -- , (show . getMKT_VAL_TOT)
                        -- , (show . getTAX_TOT)
                        -- , getFORFEIT_LAND_IND
                        -- , getBUILD_YR
                        -- , getMUNIC_CD
                        -- , getMUNIC_NM

                        -- , (T.pack . show . getMktValTot)
                        -- , (T.pack . show . getTaxTot)
                        --   TODO: , getAbstrTorrensCode
                        --   TODO: , getTorrensType
                        --   TODO: , getCondoNumber
                        --   TODO: , getContigInd1
                        --   TODO: , getCoopInd
                        --   TODO: , (T.pack . show . getNetTaxCapacity)
                        --   TODO: , (T.pack . show . getEstBldgMktVal1)
                        --   TODO: , (T.pack . show . getEstBldgMktVal2)
                        --   TODO: , (T.pack . show . getEstBldgMktVal3)
                        --   TODO: , (T.pack . show . getEstBldgMktVal4)
                        --   TODO: , (T.pack . show . getEstLandMktVal1)
                        --   TODO: , (T.pack . show . getEstLandMktVal2)
                        --   TODO: , (T.pack . show . getEstLandMktVal3)
                        --   TODO: , (T.pack . show . getEstLandMktVal4)
                        --   TODO: , (T.pack . show . getFeatureCode)
                        --   TODO: , getFracHouseNumber
                        --   TODO: , getMailingMunicCode
                        --   TODO: , getMailingMunicName
                        --   TODO: , getMetesBnds1
                        --   TODO: , getMetesBnds2
                        --   TODO: , getMetesBnds3
                        --   TODO: , getMetesBnds4
                        --   TODO: , getMoreMetesBndsInd
                        --   TODO: , getMultiAddrInd
                        --   TODO: , (T.pack . show . getObjectId)
                        --   TODO: -- , getParcelArea
                        --   TODO: , getPidText
                        --   TODO: , getPropertyStatusCd
                        --   TODO: , getPropertyTypeCd1
                        --   TODO: , getPropertyTypeCd1Name
                        --   TODO: , getPropertyTypeCd2
                        --   TODO: , getPropertyTypeCd3
                        --   TODO: , getPropertyTypeCd4
                        --   TODO: , getSaleCode
                        --   TODO: , getSaleCodeName
                        --   TODO: , getSaleDate
                        --   TODO: , (T.pack . show . getSalePrice)
                        --   TODO: , getSchoolDistNo
                        --   TODO: , getSewerDistNo
                        --   TODO: , (T.pack . show . getStateCd)
                        --   TODO: -- , getShapeArea :: !Text
                        --   TODO: -- , getShapeLen :: !Text
                        --   TODO: , getTifProjectNumber
                        --   TODO: , getWatershedNumber
                        ]

      row_fields = fmap toField $ [f attrs | f <- field_accessors]
      attrs = featureAttributes feat

