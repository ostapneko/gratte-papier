{-# LANGUAGE OverloadedStrings #-}

module Gratte.Document where

import qualified Data.Text                  as T
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Applicative
import Control.Monad

data Document = Document {
    hash     :: DocumentHash
  , filepath :: FilePath
  , tags     :: [Tag]
  , freeText :: Maybe T.Text
  } deriving (Show, Read)

newtype DocumentHash = DocumentHash String deriving (Show, Read)

instance ToJSON DocumentHash where
  toJSON (DocumentHash h) = toJSON $ T.pack h

instance FromJSON DocumentHash where
  parseJSON (String h) = return $ DocumentHash $ T.unpack h
  parseJSON _          = mzero

newtype Tag = Tag String deriving (Show, Eq, Read)

instance ToJSON Tag where
  toJSON (Tag t) = toJSON $ T.pack t

instance FromJSON Tag where
  parseJSON (String t) = return $ Tag $ T.unpack t
  parseJSON _          = mzero

toText :: Tag -> String
toText (Tag t) = t

instance ToJSON Document where
  toJSON (Document _ fp ts ft) =
    object [
        "filepath"  .= String (T.pack fp)
      , "tags"      .= toJSON ts
      , "free_text" .= toJSON ft
      ]

toPayload :: Document -> String
toPayload = BS.unpack . encode . toJSON

instance FromJSON Document where
  parseJSON (Object v) =
    Document <$> v .: "_id"
             <*> (v .: "_source" >>= (.: "filepath"))
             <*> (v .: "_source" >>= (.: "tags"))
             <*> (v .: "_source" >>= (.: "free_text"))
  parseJSON _          = mzero

data SearchResult = SearchResult Hits
instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "hits"
  parseJSON _          = mzero

data Hits = Hits [Document]
instance FromJSON Hits where
  parseJSON (Object v) = Hits <$> v .: "hits"
  parseJSON _          = mzero
