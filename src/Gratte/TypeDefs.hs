{-# LANGUAGE OverloadedStrings #-}

module Gratte.TypeDefs (
  module Gratte.TypeDefs
) where

import           Data.Monoid
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

newtype Timestamp    = Timestamp String
instance ToJSON Timestamp where
  toJSON (Timestamp t) = toJSON $ pack t

data Mode = AddMode | QueryMode

data Document = Document {
    timestamp :: Timestamp
  , filepath  :: FilePath
  , tags      :: [Tag]
  , freeText  :: Text
  }
instance ToJSON Document where
  toJSON (Document _ fp ts ft) =
    object [
        "filepath"  .= String (pack fp)
      , "tags"      .= toJSON ts
      , "free_text" .= String ft
      ]

newtype BulkEntry = BulkEntry Document

toByteString :: BulkEntry -> BS.ByteString
toByteString (BulkEntry doc) = BS.unlines [header, docJSON]
   where header = "{ \"index\": { \"_index\" : \"gratte\", \"_type\" : \"document\", \"_id\" : \""
                  <> BS.pack docTime
                  <> "\" } }"
         docJSON           = encode . toJSON $ doc
         Timestamp docTime = timestamp doc
