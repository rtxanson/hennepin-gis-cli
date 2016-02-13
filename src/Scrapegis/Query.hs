{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Query where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import qualified Data.ByteString.Lazy.Char8 as D8
import Data.Text as T
import Data.List as L

import Scrapegis.Hennepin as Henn
-- import Scrapegis.MockHennepin as Mock
import Scrapegis.Types
import Scrapegis.Export

makeQueryString :: String -> String -> String
makeQueryString "zip"      aarg = "ZIP_CD = '" ++ aarg ++ "'"
makeQueryString "owner"    aarg = "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "taxpayer" aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "names"    aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "' OR " ++
                                  "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "city"     _ = "MUNIC_CD = '01'" -- (minneapolis)
makeQueryString "pid"      aarg = "PID = '" ++ aarg ++ "'"
makeQueryString _ _ = ""

-- configured process
runQuery :: FilePath -> [Char] -> IO ()
runQuery output_file q = do
     hPutStrLn stderr $ "  Querying with: " ++ q
     records <- Henn.getHenCountyRecords (T.pack q)
     let recs = featuresToCSV $ concatenateFeatures records
     let header_str = D8.pack $ L.intercalate ("," :: String) feature_header_cols
     if output_file == "stdout"
         then do
             D8.putStrLn header_str
             D8.putStrLn recs
         else do
             h <- openFile output_file WriteMode
             D8.hPut h header_str
             D8.hPut h recs
             hClose h
             hPutStrLn stderr $ "Written to: " ++ output_file

