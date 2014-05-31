{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature
    , justFeatures
    , FeatureLookup
    , concatenateFeatures
    , getIDList
    , getFeatures
    , featureAttributes
    , feature_header_cols
    ) where

import Data.Text as T
import Data.List as L
-- import Data.Map as M

import Data.Aeson
import Control.Applicative
import Data.Csv (toRecord, toField, ToRecord, record)

import Control.Monad (mzero)

-- TODO: aim for this ordering
--
-- "PID","HOUSE_NO","STREET_NM","PROCESSED_ADDR","ZIP_CD","OWNER_NM","TAXPAYER_NM","TAXPAYER_NM_1","TAXPAYER_NM_2","TAXPAYER_NM_3","ABBREV_ADDN_NM","BLOCK","LOT","HMSTD_CD1","HMSTD_CD1_NAME","ADDITION_NO","MKT_VAL_TOT","TAX_TOT","FORFEIT_LAND_IND","BUILD_YR","ABSTR_TORRENS_CD","TORRENS_TYP","CONDO_NO","CONTIG_IND1","CO_OP_IND","MUNIC_CD","MUNIC_NM","NET_TAX_CAPACITY","EST_BLDG_MKT_VAL1","EST_BLDG_MKT_VAL2","EST_BLDG_MKT_VAL3","EST_BLDG_MKT_VAL4","EST_LAND_MKT_VAL1","EST_LAND_MKT_VAL2","EST_LAND_MKT_VAL3","EST_LAND_MKT_VAL4","FEATURECODE","FRAC_HOUSE_NO","MAILING_MUNIC_CD","MAILING_MUNIC_NM","METES_BNDS1","METES_BNDS2","METES_BNDS3","METES_BNDS4","MORE_METES_BNDS_IND","MULTI_ADDR_IND","OBJECTID","PARCEL_AREA","PID_TEXT","PROPERTY_STATUS_CD","PROPERTY_TYPE_CD1","PROPERTY_TYPE_CD1_NAME","PROPERTY_TYPE_CD2","PROPERTY_TYPE_CD3","PROPERTY_TYPE_CD4","SALE_CODE","SALE_CODE_NAME","SALE_DATE","SALE_PRICE","SCHOOL_DIST_NO","SEWER_DIST_NO","STATE_CD","Shape.area","Shape.len","TIF_PROJECT_NO","WATERSHED_NO"

-- | Each JSON result contains a list of `Feature` objects, which contains both
-- | attributes and geographical information. FeatureAttributes handles only
-- | the attribute section.

data FeatureAttributes = FeatureAttributes {
      getObjectPID :: T.Text
    , getHouseNumber :: Integer
    , getStreetName :: T.Text
    , getZipCode :: T.Text
    , getOwnerName :: T.Text
    , getTaxpayerName :: T.Text
    , getTaxpayerName1 :: T.Text
    , getTaxpayerName2 :: T.Text
    , getTaxpayerName3 :: T.Text
    , getAbbrevAddnName :: T.Text
    , getBlock :: T.Text
    , getLot :: T.Text
    , getHmstdCode1 :: T.Text
    , getHmstdCode1Name :: T.Text
    , getAdditionNo :: T.Text
    , getMktValTot :: Integer
    , getTaxTot :: Integer
    , getForfeitLandInd :: T.Text
    , getBuildYear :: T.Text
    , getMunicCode :: !Text
    , getMunicName :: !Text
    -- , getAbstr_torrens_cd :: !Text
    -- , getTorrens_typ :: !Text
    -- , getCondo_no :: !Text
    -- , getContig_ind1 :: !Text
    -- , getCo_op_ind :: !Text
    -- , getMunic_cd :: !Text
    -- , getMunic_nm :: !Text
    -- , getNet_tax_capacity :: !Text
    -- , getEst_bldg_mkt_val1 :: !Text
    -- , getEst_bldg_mkt_val2 :: !Text
    -- , getEst_bldg_mkt_val3 :: !Text
    -- , getEst_bldg_mkt_val4 :: !Text
    -- , getEst_land_mkt_val1 :: !Text
    -- , getEst_land_mkt_val2 :: !Text
    -- , getEst_land_mkt_val3 :: !Text
    -- , getEst_land_mkt_val4 :: !Text
    -- , getFeaturecode :: !Text
    -- , getFrac_house_no :: !Text
    -- , getMailing_munic_cd :: !Text
    -- , getMailing_munic_nm :: !Text
    -- , getMetes_bnds1 :: !Text
    -- , getMetes_bnds2 :: !Text
    -- , getMetes_bnds3 :: !Text
    -- , getMetes_bnds4 :: !Text
    -- , getMore_metes_bnds_ind :: !Text
    -- , getMulti_addr_ind :: !Text
    -- , getObjectid :: !Text
    -- , getParcel_area :: !Text
    -- , getPid_text :: !Text
    -- , getProperty_status_cd :: !Text
    -- , getProperty_type_cd1 :: !Text
    -- , getProperty_type_cd1_name :: !Text
    -- , getProperty_type_cd2 :: !Text
    -- , getProperty_type_cd3 :: !Text
    -- , getProperty_type_cd4 :: !Text
    -- , getSale_code :: !Text
    -- , getSale_code_name :: !Text
    -- , getSale_date :: !Text
    -- , getSale_price :: !Text
    -- , getSchool_dist_no :: !Text
    -- , getSewer_dist_no :: !Text
    -- , getState_cd :: !Text
    -- , getShapeArea :: !Text
    -- , getShapeLen :: !Text
    -- , getTif_project_no :: !Text
    -- , getWatershed_no :: !Text
    } deriving (Show)

-- | Control parsing the JSON into a Haskell data type.

instance FromJSON FeatureAttributes where
  parseJSON (Object o) = 
    FeatureAttributes <$> (o .: "PID")
                      <*> (o .: "HOUSE_NO")
                      <*> clean "STREET_NM"
                      <*> (o .: "ZIP_CD")
                      <*> clean "OWNER_NM"
                      <*> clean "TAXPAYER_NM"
                      <*> clean "TAXPAYER_NM_1"
                      <*> clean "TAXPAYER_NM_2"
                      <*> clean "TAXPAYER_NM_3"
                      <*> clean "ABBREV_ADDN_NM"
                      <*> clean "BLOCK"
                      <*> clean "LOT"
                      <*> (o .: "HMSTD_CD1")
                      <*> clean "HMSTD_CD1_NAME"
                      <*> clean "ADDITION_NO"
                      <*> (o .: "MKT_VAL_TOT")
                      <*> (o .: "TAX_TOT")
                      <*> clean "FORFEIT_LAND_IND"
                      <*> (o .: "BUILD_YR")
                      <*> (o .: "MUNIC_CD")
                      <*> clean "MUNIC_NM"
                      -- <*> (o .: "ABSTR_TORRENS_CD")
                      -- <*> (o .: "TORRENS_TYP")
                      -- <*> (o .: "CONDO_NO")
                      -- <*> (o .: "CONTIG_IND1")
                      -- <*> (o .: "CO_OP_IND")
                      -- <*> (o .: "NET_TAX_CAPACITY")
                      -- <*> (o .: "EST_BLDG_MKT_VAL1")
                      -- <*> (o .: "EST_BLDG_MKT_VAL2")
                      -- <*> (o .: "EST_BLDG_MKT_VAL3")
                      -- <*> (o .: "EST_BLDG_MKT_VAL4")
                      -- <*> (o .: "EST_LAND_MKT_VAL1")
                      -- <*> (o .: "EST_LAND_MKT_VAL2")
                      -- <*> (o .: "EST_LAND_MKT_VAL3")
                      -- <*> (o .: "EST_LAND_MKT_VAL4")
                      -- <*> (o .: "FEATURECODE")
                      -- <*> (o .: "FRAC_HOUSE_NO")
                      -- <*> (o .: "MAILING_MUNIC_CD")
                      -- <*> (o .: "MAILING_MUNIC_NM")
                      -- <*> (o .: "METES_BNDS1")
                      -- <*> (o .: "METES_BNDS2")
                      -- <*> (o .: "METES_BNDS3")
                      -- <*> (o .: "METES_BNDS4")
                      -- <*> (o .: "MORE_METES_BNDS_IND")
                      -- <*> (o .: "MULTI_ADDR_IND")
                      -- <*> (o .: "OBJECTID")
                      -- <*> (o .: "PARCEL_AREA")
                      -- <*> (o .: "PID_TEXT")
                      -- <*> (o .: "PROPERTY_STATUS_CD")
                      -- <*> (o .: "PROPERTY_TYPE_CD1")
                      -- <*> (o .: "PROPERTY_TYPE_CD1_NAME")
                      -- <*> (o .: "PROPERTY_TYPE_CD2")
                      -- <*> (o .: "PROPERTY_TYPE_CD3")
                      -- <*> (o .: "PROPERTY_TYPE_CD4")
                      -- <*> (o .: "SALE_CODE")
                      -- <*> (o .: "SALE_CODE_NAME")
                      -- <*> (o .: "SALE_DATE")
                      -- <*> (o .: "SALE_PRICE")
                      -- <*> (o .: "SCHOOL_DIST_NO")
                      -- <*> (o .: "SEWER_DIST_NO")
                      -- <*> (o .: "STATE_CD")
                      -- <*> (o .: "Shape.area")
                      -- <*> (o .: "Shape.len")
                      -- <*> (o .: "TIF_PROJECT_NO")
                      -- <*> (o .: "WATERSHED_NO")
    where
      -- needs special handling because there's tons of crap whitespace
      clean x = T.strip <$> o .: x

  parseJSON  _ = mzero

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
                      , "OWNER_NM"
                      , "TAXPAYER_NM"
                      , "TAXPAYER_NM_1"
                      , "TAXPAYER_NM_2"
                      , "TAXPAYER_NM_3"
                      , "ABBREV_ADDN_NM"
                      , "BLOCK"
                      , "LOT"
                      , "HMSTD_CD1"
                      , "HMSTD_CD1_NAME"
                      , "ADDITION_NO"
                      , "MKT_VAL_TOT"
                      , "TAX_TOT"
                      , "FORFEIT_LAND_IND"
                      , "BUILD_YR"
                      , "MUNIC_CD"
                      , "MUNIC_NM"
                      ]

instance ToRecord Feature where
  toRecord feat = record row_fields
    where
      -- TODO:
      --   there's more data cleaning to be done, probably want to combine
      --   taxpayer names, and address and such

      -- new fields go here
      field_accessors = [ getObjectPID
                        , (T.pack . show . getHouseNumber)
                        , getStreetName
                        , getZipCode
                        , getOwnerName
                        , getTaxpayerName
                        , getTaxpayerName1
                        , getTaxpayerName2
                        , getTaxpayerName3
                        , getAbbrevAddnName
                        , getBlock
                        , getLot
                        , getHmstdCode1
                        , getHmstdCode1Name
                        , getAdditionNo
                        , (T.pack . show . getMktValTot)
                        , (T.pack . show . getTaxTot)
                        , getForfeitLandInd
                        , getBuildYear
                        , getMunicCode
                        , getMunicName
                        ]

      row_fields = fmap toField $ [f attrs | f <- field_accessors]
      attrs = featureAttributes feat

