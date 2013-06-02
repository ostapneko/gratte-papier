{-# LANGUAGE OverloadedStrings #-}

module Gratte.TypeDefs (
  module Gratte.TypeDefs
) where

import qualified Data.Text                  as T
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Applicative
import Control.Monad

newtype EsHost = EsHost String
newtype Prefix = Prefix String

newtype Hash   = Hash String
instance ToJSON Hash where
  toJSON (Hash h) = toJSON $ T.pack h
instance FromJSON Hash where
  parseJSON (String h) = return $ Hash $ T.unpack h
  parseJSON _          = mzero

newtype Tag    = Tag String
instance ToJSON Tag where
  toJSON (Tag t) = toJSON $ T.pack t
instance FromJSON Tag where
  parseJSON (String t) = return $ Tag $ T.unpack t
  parseJSON _          = mzero

data Mode = AddMode | QueryMode

data Document = Document {
    hash     :: Hash
  , filepath :: FilePath
  , tags     :: [Tag]
  , freeText :: T.Text
  }
instance ToJSON Document where
  toJSON (Document _ fp ts ft) =
    object [
        "filepath"  .= String (T.pack fp)
      , "tags"      .= toJSON ts
      , "free_text" .= String ft
      ]
instance FromJSON Document where
  parseJSON (Object v) =
    Document <$> v .: "_id"
             <*> (v .: "_source" >>= (.: "filepath"))
             <*> (v .: "_source" >>= (.: "tags"))
             <*> (v .: "_source" >>= (.: "free_text"))
  parseJSON _          = mzero

toPayload :: Document -> String
toPayload = BS.unpack . encode . toJSON

data SearchResult = SearchResult Hits
instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "hits"
  parseJSON _          = mzero

data Hits = Hits [Document]
instance FromJSON Hits where
  parseJSON (Object v) = Hits <$> v .: "hits"
  parseJSON _          = mzero
