{-# LANGUAGE OverloadedStrings #-}

module OldDocument where

import qualified Data.Text                  as T

import Gratte.Tag

data OldDocument = OldDocument {
    hash     :: DocumentHash
  , filepath :: FilePath
  , tags     :: [Tag]
  , freeText :: Maybe T.Text
  } deriving (Show, Read)

newtype DocumentHash = DocumentHash String deriving (Show, Read)
