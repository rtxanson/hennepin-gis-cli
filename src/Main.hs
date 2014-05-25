{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Scrapegis        (run)
-- import Scrapegis.Option (getOptions, getMode, usage)

import Scrapegis
-- import Scrapegis.Hennepin as Henn
import Scrapegis.MockHennepin as Mock
import Scrapegis.Types

-- Option parsing
import Control.Monad (when)
import Data.Char (toUpper)
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command, argument, longOption)

import Data.Text as T

-- TODO: FeatureLookup -> CSV

import Data.Csv (toRecord, encode, Record)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as D8

import Control.Applicative

import Data.List as L

query :: T.Text
query = "ZIP_CD = '55401'"

-- need a good cassava encoding example to run by
-- toCSV :: Maybe FeatureLookup -> B.ByteString
-- toCSV Nothing = []
-- toCSV (Just fl) = recs
--   where features = getFeatures fl
--         recs = encode $ toRecord <$> features

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ fmap toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

main = do
  opts <- optionsWithUsageFile "Usage.txt"
  -- print opts

  when (opts `isPresent` (command "query")) $ do
    query_string <- opts `getArg` (argument "query_string")
    let dataSource = if opts `isPresent` (longOption "mock")
                           then Mock.getHenCountyRecords
                           else Mock.getHenCountyRecords

    records <- dataSource query_string
    let recs = queryToCSV records
    D8.putStrLn recs

-- main = do
--     args <- getArgs
--  
--     -- Parse options, getting a list of option actions
--     let (actions, nonOptions, errors) = getOpt RequireOrder options args
--  
--     -- Here we thread startOptions through all supplied option actions
--     opts <- foldl (>>=) (return startOptions) actions
--  
--     let Options { optVerbose = verbose
--                 , optInput = input
--                 , optOutput = output   } = opts
--  
--     when verbose (hPutStrLn stderr "Hello!")
--  
--     input >>= output
