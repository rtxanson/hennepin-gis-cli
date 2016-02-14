{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature(..)
    , FeatureLookup(..)
    , FeatureAttributes(..)
    , justFeatures
    , concatenateFeatures
    , getIDList
    , feature_header_cols
    ) where

import Data.Text as T
import Data.List as L
-- import Data.Map as M

import Data.Aeson
import Control.Applicative
import Data.Csv (toRecord, toField, ToRecord, record)

import Control.Monad (mzero)

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
    , getMunicCode :: T.Text
    , getMunicName :: T.Text
    , getAbstrTorrensCode :: T.Text
    , getTorrensType :: T.Text
    , getCondoNumber :: T.Text
    , getContigInd1 :: T.Text
    , getCoopInd :: T.Text
    , getNetTaxCapacity :: Integer
    , getEstBldgMktVal1 :: Integer
    , getEstBldgMktVal2 :: Integer
    , getEstBldgMktVal3 :: Integer
    , getEstBldgMktVal4 :: Integer
    , getEstLandMktVal1 :: Integer
    , getEstLandMktVal2 :: Integer
    , getEstLandMktVal3 :: Integer
    , getEstLandMktVal4 :: Integer
    , getFeatureCode :: Integer
    , getFracHouseNumber :: T.Text
    , getMailingMunicCode :: T.Text
    , getMailingMunicName :: T.Text
    , getMetesBnds1 :: T.Text
    , getMetesBnds2 :: T.Text
    , getMetesBnds3 :: T.Text
    , getMetesBnds4 :: T.Text
    , getMoreMetesBndsInd :: T.Text
    , getMultiAddrInd :: T.Text
    , getObjectId :: Integer
    -- TODO: what type is this actually? , getParcelArea :: T.Text
    , getPidText :: T.Text
    , getPropertyStatusCd :: T.Text
    , getPropertyTypeCd1 :: T.Text
    , getPropertyTypeCd1Name :: T.Text
    , getPropertyTypeCd2 :: T.Text
    , getPropertyTypeCd3 :: T.Text
    , getPropertyTypeCd4 :: T.Text
    , getSaleCode :: T.Text
    , getSaleCodeName :: T.Text
    , getSaleDate :: T.Text
    , getSalePrice :: Integer
    , getSchoolDistNo :: T.Text
    , getSewerDistNo :: T.Text
    , getStateCd :: Integer
    -- TODO: expecting this needs another type - , getShapeArea :: T.Text
    -- TODO: expecting this needs another type - , getShapeLen :: T.Text
    , getTifProjectNumber :: T.Text
    , getWatershedNumber :: T.Text
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
                      <*> clean "ABSTR_TORRENS_CD"
                      <*> clean "TORRENS_TYP"
                      <*> clean "CONDO_NO"
                      <*> clean "CONTIG_IND1"
                      <*> clean "CO_OP_IND"
                      <*> (o .: "NET_TAX_CAPACITY")
                      <*> (o .: "EST_BLDG_MKT_VAL1")
                      <*> (o .: "EST_BLDG_MKT_VAL2")
                      <*> (o .: "EST_BLDG_MKT_VAL3")
                      <*> (o .: "EST_BLDG_MKT_VAL4")
                      <*> (o .: "EST_LAND_MKT_VAL1")
                      <*> (o .: "EST_LAND_MKT_VAL2")
                      <*> (o .: "EST_LAND_MKT_VAL3")
                      <*> (o .: "EST_LAND_MKT_VAL4")
                      <*> (o .: "FEATURECODE")
                      <*> clean "FRAC_HOUSE_NO"
                      <*> clean "MAILING_MUNIC_CD"
                      <*> clean "MAILING_MUNIC_NM"
                      <*> clean "METES_BNDS1"
                      <*> clean "METES_BNDS2"
                      <*> clean "METES_BNDS3"
                      <*> clean "METES_BNDS4"
                      <*> clean "MORE_METES_BNDS_IND"
                      <*> clean "MULTI_ADDR_IND"
                      <*> (o .: "OBJECTID")
                      -- <*> clean "PARCEL_AREA"
                      <*> clean "PID_TEXT"
                      <*> clean "PROPERTY_STATUS_CD"
                      <*> clean "PROPERTY_TYPE_CD1"
                      <*> clean "PROPERTY_TYPE_CD1_NAME"
                      <*> clean "PROPERTY_TYPE_CD2"
                      <*> clean "PROPERTY_TYPE_CD3"
                      <*> clean "PROPERTY_TYPE_CD4"
                      <*> clean "SALE_CODE"
                      <*> clean "SALE_CODE_NAME"
                      <*> (o .: "SALE_DATE")
                      <*> (o .: "SALE_PRICE")
                      <*> clean "SCHOOL_DIST_NO"
                      <*> clean "SEWER_DIST_NO"
                      <*> (o .: "STATE_CD")
                      -- <*> (o .: "Shape.area")
                      -- <*> (o .: "Shape.len")
                      <*> clean "TIF_PROJECT_NO"
                      <*> (o .: "WATERSHED_NO")
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
                      , "ABSTR_TORRENS_CD"
                      , "TORRENS_TYP"
                      , "CONDO_NO"
                      , "CONTIG_IND1"
                      , "CO_OP_IND"
                      , "NET_TAX_CAPACITY"
                      , "EST_BLDG_MKT_VAL1"
                      , "EST_BLDG_MKT_VAL2"
                      , "EST_BLDG_MKT_VAL3"
                      , "EST_BLDG_MKT_VAL4"
                      , "EST_LAND_MKT_VAL1"
                      , "EST_LAND_MKT_VAL2"
                      , "EST_LAND_MKT_VAL3"
                      , "EST_LAND_MKT_VAL4"
                      , "FEATURECODE"
                      , "FRAC_HOUSE_NO"
                      , "MAILING_MUNIC_CD"
                      , "MAILING_MUNIC_NM"
                      , "METES_BNDS1"
                      , "METES_BNDS2"
                      , "METES_BNDS3"
                      , "METES_BNDS4"
                      , "MORE_METES_BNDS_IND"
                      , "MULTI_ADDR_IND"
                      , "OBJECTID"
                      -- <*> clean "PARCEL_AREA"
                      , "PID_TEXT"
                      , "PROPERTY_STATUS_CD"
                      , "PROPERTY_TYPE_CD1"
                      , "PROPERTY_TYPE_CD1_NAME"
                      , "PROPERTY_TYPE_CD2"
                      , "PROPERTY_TYPE_CD3"
                      , "PROPERTY_TYPE_CD4"
                      , "SALE_CODE"
                      , "SALE_CODE_NAME"
                      , "SALE_DATE"
                      , "SALE_PRICE"
                      , "SCHOOL_DIST_NO"
                      , "SEWER_DIST_NO"
                      , "STATE_CD"
                      -- <*> (o .: "Shape.area")
                      -- <*> (o .: "Shape.len")
                      , "TIF_PROJECT_NO"
                      , "WATERSHED_NO"
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
                        , getAbstrTorrensCode
                        , getTorrensType
                        , getCondoNumber
                        , getContigInd1
                        , getCoopInd
                        , (T.pack . show . getNetTaxCapacity)
                        , (T.pack . show . getEstBldgMktVal1)
                        , (T.pack . show . getEstBldgMktVal2)
                        , (T.pack . show . getEstBldgMktVal3)
                        , (T.pack . show . getEstBldgMktVal4)
                        , (T.pack . show . getEstLandMktVal1)
                        , (T.pack . show . getEstLandMktVal2)
                        , (T.pack . show . getEstLandMktVal3)
                        , (T.pack . show . getEstLandMktVal4)
                        , (T.pack . show . getFeatureCode)
                        , getFracHouseNumber
                        , getMailingMunicCode
                        , getMailingMunicName
                        , getMetesBnds1
                        , getMetesBnds2
                        , getMetesBnds3
                        , getMetesBnds4
                        , getMoreMetesBndsInd
                        , getMultiAddrInd
                        , (T.pack . show . getObjectId)
                        -- , getParcelArea
                        , getPidText
                        , getPropertyStatusCd
                        , getPropertyTypeCd1
                        , getPropertyTypeCd1Name
                        , getPropertyTypeCd2
                        , getPropertyTypeCd3
                        , getPropertyTypeCd4
                        , getSaleCode
                        , getSaleCodeName
                        , getSaleDate
                        , (T.pack . show . getSalePrice)
                        , getSchoolDistNo
                        , getSewerDistNo
                        , (T.pack . show . getStateCd)
                        -- , getShapeArea :: !Text
                        -- , getShapeLen :: !Text
                        , getTifProjectNumber
                        , getWatershedNumber
                        ]

      row_fields = fmap toField $ [f attrs | f <- field_accessors]
      attrs = featureAttributes feat

instance ToJSON Feature where
  toJSON feature = obj
    where
        feat = featureAttributes feature
        obj = object [ "PID" .=                    getObjectPID feat
                     , "HOUSE_NO" .=               (T.pack . show . getHouseNumber)      feat
                     , "STREET_NM" .=              getStreetName       feat
                     , "ZIP_CD" .=                 getZipCode    feat
                     , "OWNER_NM" .=               getOwnerName      feat
                     , "TAXPAYER_NM" .=            getTaxpayerName         feat
                     , "TAXPAYER_NM_1" .=          getTaxpayerName1           feat
                     , "TAXPAYER_NM_2" .=          getTaxpayerName2           feat
                     , "TAXPAYER_NM_3" .=          getTaxpayerName3           feat
                     , "ABBREV_ADDN_NM" .=         getAbbrevAddnName            feat
                     , "BLOCK" .=                  getBlock   feat
                     , "LOT" .=                    getLot feat
                     , "HMSTD_CD1" .=              getHmstdCode1       feat
                     , "HMSTD_CD1_NAME" .=         getHmstdCode1Name            feat
                     , "ADDITION_NO" .=            getAdditionNo         feat
                     , "MKT_VAL_TOT" .=            (T.pack . show . getMktValTot)         feat
                     , "TAX_TOT" .=                (T.pack . show . getTaxTot)     feat
                     , "FORFEIT_LAND_IND" .=       getForfeitLandInd              feat
                     , "BUILD_YR" .=               getBuildYear      feat
                     , "MUNIC_CD" .=               getMunicCode      feat
                     , "MUNIC_NM" .=               getMunicName      feat
                     , "ABSTR_TORRENS_CD" .=       getAbstrTorrensCode              feat
                     , "TORRENS_TYP" .=            getTorrensType         feat
                     , "CONDO_NO" .=               getCondoNumber      feat
                     , "CONTIG_IND1" .=            getContigInd1         feat
                     , "CO_OP_IND" .=              getCoopInd       feat
                     , "NET_TAX_CAPACITY" .=       (T.pack . show . getNetTaxCapacity)              feat
                     , "EST_BLDG_MKT_VAL1" .=      (T.pack . show . getEstBldgMktVal1)               feat
                     , "EST_BLDG_MKT_VAL2" .=      (T.pack . show . getEstBldgMktVal2)               feat
                     , "EST_BLDG_MKT_VAL3" .=      (T.pack . show . getEstBldgMktVal3)               feat
                     , "EST_BLDG_MKT_VAL4" .=      (T.pack . show . getEstBldgMktVal4)               feat
                     , "EST_LAND_MKT_VAL1" .=      (T.pack . show . getEstLandMktVal1)               feat
                     , "EST_LAND_MKT_VAL2" .=      (T.pack . show . getEstLandMktVal2)               feat
                     , "EST_LAND_MKT_VAL3" .=      (T.pack . show . getEstLandMktVal3)               feat
                     , "EST_LAND_MKT_VAL4" .=      (T.pack . show . getEstLandMktVal4)               feat
                     , "FEATURECODE" .=            (T.pack . show . getFeatureCode)         feat
                     , "FRAC_HOUSE_NO" .=          getFracHouseNumber           feat
                     , "MAILING_MUNIC_CD" .=       getMailingMunicCode              feat
                     , "MAILING_MUNIC_NM" .=       getMailingMunicName              feat
                     , "METES_BNDS1" .=            getMetesBnds1         feat
                     , "METES_BNDS2" .=            getMetesBnds2         feat
                     , "METES_BNDS3" .=            getMetesBnds3         feat
                     , "METES_BNDS4" .=            getMetesBnds4         feat
                     , "MORE_METES_BNDS_IND" .=    getMoreMetesBndsInd                 feat
                     , "MULTI_ADDR_IND" .=         getMultiAddrInd            feat
                     , "OBJECTID" .=               (T.pack . show . getObjectId)      feat
                     -- , "PARCEL_AREA" .=         -- , getParcelArea            feat
                     , "PID_TEXT" .=               getPidText      feat
                     , "PROPERTY_STATUS_CD" .=     getPropertyStatusCd                feat
                     , "PROPERTY_TYPE_CD1" .=      getPropertyTypeCd1               feat
                     , "PROPERTY_TYPE_CD1_NAME" .= getPropertyTypeCd1Name                    feat
                     , "PROPERTY_TYPE_CD2" .=      getPropertyTypeCd2               feat
                     , "PROPERTY_TYPE_CD3" .=      getPropertyTypeCd3               feat
                     , "PROPERTY_TYPE_CD4" .=      getPropertyTypeCd4               feat
                     , "SALE_CODE" .=              getSaleCode       feat
                     , "SALE_CODE_NAME" .=         getSaleCodeName            feat
                     , "SALE_DATE" .=              getSaleDate       feat
                     , "SALE_PRICE" .=             (T.pack . show . getSalePrice)        feat
                     , "SCHOOL_DIST_NO" .=         getSchoolDistNo            feat
                     , "SEWER_DIST_NO" .=          getSewerDistNo           feat
                     , "STATE_CD" .=               (T.pack . show . getStateCd)      feat
                     -- , "Shape.area" .=          -- , getShapeArea :: !Text           feat
                     -- , "Shape.len" .=           -- , getShapeLen :: !Text          feat
                     , "TIF_PROJECT_NO" .=         getTifProjectNumber            feat
                     , "WATERSHED_NO" .=           getWatershedNumber          feat
                     ]
  
  
