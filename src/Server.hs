{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Control.Monad.Gratte

import qualified Data.Aeson as A

import Web.Scotty

import Gratte.Options
import Gratte.Search


main :: IO ()
main = scotty 3000 $
  get "/search" $ do
    q <- param "q"
    docs <- liftIO $ withGratte (webSearchOptions q) $ getDocs q
    json $ A.object [ "documents" A..= docs ]
