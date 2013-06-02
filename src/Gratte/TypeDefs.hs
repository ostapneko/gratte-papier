{-# LANGUAGE OverloadedStrings #-}

module Gratte.TypeDefs (
  module Gratte.TypeDefs
) where

import           Data.Text (Text, pack)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

newtype EsHost = EsHost String
newtype Prefix = Prefix String

newtype Hash   = Hash String
instance ToJSON Hash where
  toJSON (Hash h) = toJSON $ pack h

newtype Tag    = Tag String
instance ToJSON Tag where
  toJSON (Tag t) = toJSON $ pack t

data Mode = AddMode | QueryMode

data Document = Document {
    hash     :: Hash
  , filepath :: FilePath
  , tags     :: [Tag]
  , freeText :: Text
  }
instance ToJSON Document where
  toJSON (Document _ fp ts ft) =
    object [
        "filepath"  .= String (pack fp)
      , "tags"      .= toJSON ts
      , "free_text" .= String ft
      ]

toPayload :: Document -> String
toPayload = BS.unpack . encode . toJSON
