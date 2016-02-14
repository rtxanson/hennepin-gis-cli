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

textToQueryResult :: B.ByteString -> Maybe IDQueryResult
textToQueryResult text = decode text :: Maybe IDQueryResult

textToFeatureLookup :: B.ByteString -> Maybe FeatureLookup
textToFeatureLookup text = decode text :: Maybe FeatureLookup

spec :: Spec
spec = do
  describe "Scrapegis.Types" $ do
    it "can parse ID Query Result JSON" $ do
      h <- openFile "test_data/object_ids.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToQueryResult instr
      let first = getIDList json !! 0
      first `shouldBe` 25218
      hClose h

    it "can parse Feature Lookup JSON" $ do
      h <- openFile "test_data/test_chunk.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToFeatureLookup instr
      let feats = getFeatures json
      let attrs = featureAttributes $ feats !! 0
      (getZipCode attrs) `shouldBe` "55414"
      hClose h


