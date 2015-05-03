{-# LANGUAGE OverloadedStrings #-}
module Gratte.SearchEngineSpec
  ( tokenizeSpec
  ) where

import Test.Hspec
import qualified Data.Set as S
import Gratte.Tag
import Gratte.SearchEngine
import Gratte.Document
import qualified Data.Text as T

docWithText :: T.Text -> Document
docWithText t = emptyDoc { docScannedText = Just t }

emptyDoc :: Document
emptyDoc = Document
          { docHash        = DocumentHash ""
          , docTitle       = DocumentTitle ""
          , docPath        = DocumentPath ""
          , docSender      = DocumentSender ""
          , docRecipient   = DocumentRecipient ""
          , docDate        = DocumentDate Nothing 2015
          , docTags        = [Tag ""]
          , docScannedText = Just ""
          }

tokenizeSpec :: IO ()
tokenizeSpec = do
  let fullDoc = emptyDoc
              { docTitle       = DocumentTitle "Title"
              , docSender      = DocumentSender "Sender"
              , docRecipient   = DocumentRecipient "Recipient"
              , docTags        = [Tag "Tag"]
              , docScannedText = Just "ScannedText"
              }

      fullTokenized = S.fromList ["Title", "Sender", "Recipient", "Tag", "ScannedText"]


  hspec $ do
    describe "tokenize" $ do
      it "returns an empty set for an empty document" $
        tokenize emptyDoc `shouldBe` S.empty
      describe "with all fields" $
        it "tokenizes all fields" $
          tokenize fullDoc `shouldBe` fullTokenized
      it "splits on whitespaces" $
        tokenize (docWithText "Test doc") `shouldBe` S.fromList ["Test", "doc"]
      it "removes all punctuation" $
        tokenize (docWithText "Test: doc") `shouldBe` S.fromList ["Test", "doc"]
      it "preserves URIs" $
        tokenize (docWithText "www.example.com/example doc") `shouldBe` S.fromList ["www.example.com/example", "doc"]

