{-# LANGUAGE OverloadedStrings #-}

module Gratte.Document where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Data.List                  as L

import           Control.Applicative
import           Control.Monad

import           Gratte.Tag

data Document = Document {
    docHash        :: DocumentHash
  , docTitle       :: DocumentTitle
  , docFilepath    :: DocumentPath
  , docTags        :: [Tag]
  , docScannedText :: Maybe T.Text
  } deriving (Show, Read)

newtype DocumentPath = DocumentPath String deriving (Show, Read)
instance ToJSON DocumentPath where
  toJSON (DocumentPath p) = toJSON $ T.pack p

instance FromJSON DocumentPath where
  parseJSON (String p) = return $ DocumentPath $ T.unpack p
  parseJSON _          = mzero

documentPathToString :: DocumentPath -> String
documentPathToString (DocumentPath p) = p

newtype DocumentTitle = DocumentTitle String deriving (Show, Read)
instance ToJSON DocumentTitle where
  toJSON (DocumentTitle t) = toJSON $ T.pack t

instance FromJSON DocumentTitle where
  parseJSON (String t) = return $ DocumentTitle $ T.unpack t
  parseJSON _          = mzero

newtype DocumentHash = DocumentHash String deriving (Show, Read)

instance ToJSON DocumentHash where
  toJSON (DocumentHash h) = toJSON $ T.pack h

instance FromJSON DocumentHash where
  parseJSON (String h) = return $ DocumentHash $ T.unpack h
  parseJSON _          = mzero

instance ToJSON Document where
  toJSON (Document _ (DocumentTitle title) (DocumentPath fp) ts ft) =
    object [
        "filepath"  .= String (T.pack fp)
      , "title"     .= String (T.pack title)
      , "tags"      .= toJSON ts
      , "free_text" .= toJSON ft
      ]

toPayload :: Document -> String
toPayload = BS.unpack . encode . toJSON

instance FromJSON Document where
  parseJSON (Object v) =
    Document <$> v .: "_id"
             <*> (v .: "_source" >>= (.: "filepath"))
             <*> (v .: "_source" >>= (.: "title"))
             <*> (v .: "_source" >>= (.: "tags"))
             <*> (v .: "_source" >>= (.:? "free_text"))
  parseJSON _          = mzero

data SearchResult = SearchResult Hits
instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "hits"
  parseJSON _          = mzero

data Hits = Hits [Document]
instance FromJSON Hits where
  parseJSON (Object v) = Hits <$> v .: "hits"
  parseJSON _          = mzero

createReport :: [Document] -> String
createReport docs =
  let firstLine = "You are about to archive the following files:"
      docDescriptions = map describeDoc docs
  in unlines $ firstLine : docDescriptions

describeDoc :: Document -> String
describeDoc doc =
  let (DocumentTitle title) = docTitle doc
      tags = docTags doc
      text = maybe "(no scanned text)" T.unpack $ docScannedText doc
  in "--------------------------------\n"
     ++ "Title: " ++ title ++ "\n"
     ++ "Tags: " ++ L.intercalate ", " (map toText tags) ++ "\n"
     ++ "Scanned text: " ++ text ++ "\n"
