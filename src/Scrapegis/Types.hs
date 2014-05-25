{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature
    , FeatureLookup
    , getIDList
    , getFeatures
    , featureAttributes
    ) where

import Data.Aeson
import Data.Map as M
import Control.Applicative
import Data.Csv (toRecord, toField, ToRecord, record)

-- TODO: all of the fields in this mapping are kind of crazy and need to be
-- handled. For now just a subset to get the concept working.

data FeatureAttributes = FeatureAttributes {
      getZipCode :: String
    , getMunicipalityName :: String
    , getObjectID :: Integer
    , getObjectPID :: String
    , getOwnerName :: String
    } deriving (Show)

instance FromJSON FeatureAttributes where
  parseJSON (Object o) = 
    FeatureAttributes <$> (o .: "ZIP_CD")
                      <*> (o .: "MUNIC_NM")
                      <*> (o .: "OBJECTID")
                      <*> (o .: "PID")
                      <*> (o .: "OWNER_NM")

data IDQueryResult = IDQueryResult { getIDList :: [Integer]
                                   } deriving (Show)

instance FromJSON IDQueryResult where
  parseJSON (Object o) = IDQueryResult <$> (o .: "objectIds")

data Feature = Feature { featureAttributes :: FeatureAttributes
                       } deriving (Show)

instance FromJSON Feature where
  parseJSON (Object o) = Feature <$> o .: "attributes"


data FeatureLookup = FeatureLookup { getFeatures :: [Feature]
                                   , displayFieldName :: String
                                   , fieldAliases :: M.Map String String
                                   , spatialReference :: M.Map String Integer
                                   , geometryType :: String
                                   } deriving (Show)

-- these all have to be in the same order as the fields above.
instance FromJSON FeatureLookup where
  parseJSON (Object o) = FeatureLookup <$> o .: "features"
                                       <*> o .: "displayFieldName"
                                       <*> o .: "fieldAliases"
                                       <*> o .: "spatialReference"
                                       <*> o .: "geometryType"


instance ToRecord FeatureLookup where
  toRecord feat = record [toField fieldname, toField fieldname]
    where
      fieldname = displayFieldName feat


instance ToRecord Feature where
  toRecord feat = record [toField owner, toField zip]
    where
      owner = getOwnerName attrs
      zip = getZipCode attrs
      attrs = featureAttributes feat

