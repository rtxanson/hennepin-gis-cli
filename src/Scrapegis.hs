{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Scrapegis 
    ( run
    ) where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import Scrapegis.Hennepin as Henn
-- import Scrapegis.MockHennepin as Mock
import Scrapegis.Types
import Scrapegis.Export

-- Option parsing
import Control.Monad (when)

import System.Console.Docopt 
import System.Environment (getArgs)

-- import Data.Vector (fromList)

-- import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8
import Data.Text as T
import Data.List as L


makeQueryString :: String -> String -> String
makeQueryString "zip"      aarg = "ZIP_CD = '" ++ aarg ++ "'"
makeQueryString "owner"    aarg = "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "taxpayer" aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "names"    aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "' OR " ++
                                  "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "city"     _ = "MUNIC_CD = '01'" -- (minneapolis)
makeQueryString "pid"      aarg = "PID = '" ++ aarg ++ "'"
makeQueryString _ _ = ""

-- TODO: with optional object KML field
-- TODO: option to specify chunk size. default, 900? 
-- TODO: output as stuff becomes available-- don't need to store in mem.
-- TODO: output geojson polygons to kml snippets: https://hackage.haskell.org/package/geojson
-- http://hackage.haskell.org/package/gps-0.2.4/docs/Data-GPS.html
-- http://hackage.haskell.org/package/proj4-hs-bindings
-- https://github.com/pavpen/proj4-hs-bindings
--
-- http://hackage.haskell.org/package/txt-sushi
-- http://hackage.haskell.org/package/csv-conduit


-- http://hackage.haskell.org/package/esqueleto-1.4.1.2/docs/Database-Esqueleto.html

patterns :: Docopt
patterns = [docoptFile|src/Usage.txt|]

run :: IO ()
run =  do
    opts <- parseArgsOrExit patterns =<< getArgs
    handleOpts opts

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

handleOpts :: Arguments -> IO ()
handleOpts opts = do

    whenCmd "query" $ do
        let query_string = getOpt "query_string"
        go query_string

    whenCmd "fetch" $ do
        whenCmd "zip" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "zip" query_arg
            go query_string

        whenCmd "owner" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "owner" query_arg
            go query_string

        whenCmd "taxpayer" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "taxpayer" query_arg
            go query_string

        whenCmd "names" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "names" query_arg
            go query_string

        whenCmd "city" $ do
            -- EDINA: 24
            -- ORONO: 38
            -- RICHFIELD: 42
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "city" query_arg

            go query_string

        whenCmd "pid" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "pid" query_arg

            go query_string

    hPutStrLn stderr $ "Done."

  where
      output_file = getArgWithDefault opts "stdout" (longOption "out")
      go q = runQuery output_file q

      -- Some docopt shortcuts
      whenCmd x = when $ opts `isPresent` (command x)
      getOpt  x =        L.concat $ opts `getAllArgs` (argument x)

      -- Options
      -- whenOpt x =        opts `isPresent` (longOption x)
      -- dataSource = if whenOpt "mock"
      --                        then Mock.getHenCountyRecords
      --                        else 
      -- post_processing = featuresToCSV
      -- post_processing = if whenOpt "csv"
      --                       then featuresToCSV
      --                       else if whenOpt "json"
      --                       	 then featuresToJSON
      --                       	 else featuresToCSV


