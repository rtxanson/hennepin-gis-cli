{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards  #-}

module Scrapegis.App
  ( AppState(..)
  , AppEnv(..)
  , AppIO
  , runAppT
  ) where

import Control.Monad.Reader
import Control.Monad.State

import Scrapegis.Types (OutputData)

-- | Parameters for the main run.

data AppEnv = AppEnv { outputFile :: FilePath 
                     , queryString :: String 
                     } deriving (Show)

data AppState = AppState { resultData :: Maybe OutputData }

type AppT m = StateT AppState (ReaderT AppEnv m)
type AppIO = AppT IO

-- | Some crazy magic; run the main app function with the state and environment
runAppT :: Monad m => AppState -> AppEnv -> AppT m a -> m a
runAppT s e f = flip runReaderT e . flip evalStateT s $ f
