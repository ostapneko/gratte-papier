{-# LANGUAGE OverloadedStrings #-}

module Gratte.Command.Serve
  ( serve
  ) where

import Control.Monad.Trans
import Control.Monad.Gratte
import Control.Applicative
import System.Environment

import qualified Data.Aeson                 as A
import           Filesystem.Path.CurrentOS  ( (</>) )
import qualified Filesystem.Path.CurrentOS  as FS

import Web.Scotty hiding (Options)
import Network.Wai.Middleware.Static

import Gratte.Options
import Gratte.Command.Search

getStaticDir :: IO FS.FilePath
getStaticDir = staticDir <$> getExecutablePath
  where staticDir str = FS.directory (FS.decodeString str) </> "static"

serve :: Options -> ServeOptions -> IO ()
serve opts serveOpts = do
  staticDir <- getStaticDir

  let staticDirStr = FS.encodeString staticDir
      p = port serveOpts
      docsFolder = folder opts

  scotty p $ do
    get "/" $ do
      setHeader "Content-Type" "text/html"
      file $ FS.encodeString $ staticDir </> "index.html"

    get "/search" $ do
      q    <- param "q"
      docs <- liftIO $ withGratte opts $ getDocs q
      json $ A.object [ "documents" A..= docs ]

    get (regex "^/docs/(.*)$") $ do
      pathSuffix <- FS.decodeString <$> param "1"
      let path = docsFolder </> pathSuffix
      file $ FS.encodeString path

    middleware $ staticPolicy (noDots >-> addBase staticDirStr)
