{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Settings
    ( multnomah_gis_host
    ) where

-- http://www3.multco.us/arcgispublic/rest/services/DART/Taxlots_Orion_Public/MapServer?f=pjson
multnomah_gis_host :: String
multnomah_gis_host = "http://www3.multco.us/arcgispublic/rest/services/DART/Taxlots_Orion_Public/MapServer/0/query"
