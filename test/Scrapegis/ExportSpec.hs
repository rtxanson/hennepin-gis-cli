{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.ExportSpec (spec) where

import Test.Hspec

-- import Test.QuickChec
import Control.Exception (evaluate)

import Scrapegis.Export
import Scrapegis.Types

import System.IO ( hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(ReadMode)
                 )

import qualified Data.ByteString.Lazy.Char8 as D8
import qualified Data.ByteString.Lazy as B

import Data.Aeson

textToQueryResult :: B.ByteString -> Either String IDQueryResult
textToQueryResult = eitherDecode

textToFeatureLookup :: B.ByteString -> Either String FeatureLookup
textToFeatureLookup = eitherDecode

textToAttributes :: B.ByteString -> Either String FeatureAttributes
textToAttributes = eitherDecode

spec :: Spec
spec = do
  describe "Scrapegis.Types" $ do

    it "can parse ID Query Result JSON" $ do
      h <- openFile "test_data/object_ids.json" ReadMode
      instr <- B.hGetContents h
      case textToQueryResult instr of
        Left err -> do
          putStrLn err
          "bbq" `shouldBe` "foo"
        Right json -> do
          let first = head (getIDList json)
          first `shouldBe` 461446
      hClose h

    it "can parse Feature Lookup Attribute JSON" $ do
      h <- openFile "test_data/test_attributes.json" ReadMode
      instr <- B.hGetContents h
      case textToAttributes instr of
         Left err -> do
              putStrLn err
              "Boop" `shouldBe` "55401"
         Right json -> do
              putStrLn $ getZIP_CD json
              getZIP_CD json `shouldBe` "55401"
      hClose h


    it "can parse Feature Lookup JSON" $ do
      h <- openFile "test_data/two_json.json" ReadMode
      instr <- B.hGetContents h
      case textToFeatureLookup instr of
         Left err -> do
              putStrLn err
              "Boop" `shouldBe` "55401"
         Right json -> do
              let feats = getFeatures json
              let attrs = featureAttributes $ head feats
              putStrLn $ getZIP_CD attrs
              getZIP_CD attrs `shouldBe` "55401"
      hClose h

  describe "Scrapegis.Export" $
    it "can turn Feature Lookup JSON to CSV" $ do

      h <- openFile "test_data/test_chunk.json" ReadMode
      instr <- B.hGetContents h

      case textToFeatureLookup instr of
        Left err -> 
          putStrLn err
        Right json -> do
          let feats = take 2 $ getFeatures json
          let csv = featuresToCSV feats
          hClose h

