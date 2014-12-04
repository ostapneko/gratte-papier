{-# LANGUAGE OverloadedStrings #-}

module Gratte.Serve
  ( serve
  ) where

import Control.Monad.Trans
import Control.Monad.Gratte

import qualified Data.Aeson as A

import Web.Scotty

import Gratte.Options
import Gratte.Search


serve :: ServeOptions -> IO ()
serve opts = do
  let p = port opts
  scotty p $
    get "/search" $ do
      q    <- param "q"
      docs <- liftIO $ withGratte (webSearchOptions q) $ getDocs q
      json $ A.object [ "documents" A..= docs ]
