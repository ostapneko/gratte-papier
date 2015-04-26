{-# LANGUAGE OverloadedStrings #-}
module Gratte.SearchEngineSpec
  ( tokenizeSpec
  ) where

import Test.Hspec
import qualified Data.Set as S
import Gratte.Tag
import Gratte.SearchEngine
import Gratte.Document

tokenizeSpec :: IO ()
tokenizeSpec = do
  let emptyDoc = Document {
    docHash        = DocumentHash ""
  , docTitle       = DocumentTitle ""
  , docPath        = DocumentPath ""
  , docSender      = DocumentSender ""
  , docRecipient   = DocumentRecipient ""
  , docDate        = DocumentDate Nothing 2015
  , docTags        = [Tag ""]
  , docScannedText = Just ""
  }

      fullDoc = emptyDoc {
        docTitle = DocumentTitle "Title"
      , docSender = DocumentSender "Sender"
      , docRecipient = DocumentRecipient "Recipient"
      , docTags = [Tag "Tag"]
      , docScannedText = Just "ScannedText"
      }

      fullTokenized = S.fromList ["Title", "Sender", "Recipient", "Tag", "ScannedText"]

  hspec $ do
    describe "tokenize" $
      it "tokenizes a string" $
        tokenize emptyDoc `shouldBe` S.empty
    describe "with all fields" $
      it "should tokenize all fields" $
        tokenize fullDoc `shouldBe` fullTokenized
    describe "whitespace separation" $
      it "should split on whitespaces" $
        tokenize (emptyDoc { docScannedText = Just "Test doc" }) `shouldBe` S.fromList ["Test", "doc"]
    describe "remove punctuation" $
      it "should remove all punctuation" $
        tokenize (emptyDoc { docScannedText = Just "Test: doc" }) `shouldBe` S.fromList ["Test", "doc"]

