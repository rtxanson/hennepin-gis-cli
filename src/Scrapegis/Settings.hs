{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Settings
    ( hennepin_gis_host
    ) where

hennepin_gis_host :: String
hennepin_gis_host = "https://gis.hennepin.us/ArcGIS/rest/services/Maps/PROPERTY/MapServer/0/query"

-- https://gis.hennepin.us/arcgis/rest/services/Maps/PROPERTY/MapServer/0/query?where=&text=&objectIds=344068,344069&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=json
