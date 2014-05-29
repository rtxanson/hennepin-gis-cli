{-# LANGUAGE OverloadedStrings #-}

module Main where

import Scrapegis (run)
import System.Console.Docopt (optionsWithUsageFile)
import Paths_scrapegis

main :: IO ()
main = getDataFileName "src/Usage.txt"
     >>= optionsWithUsageFile
     >>= run
