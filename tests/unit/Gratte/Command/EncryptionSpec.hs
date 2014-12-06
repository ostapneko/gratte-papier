{-# LANGUAGE OverloadedStrings #-}
module Gratte.Command.EncryptionSpec
  ( encryptionSpec
  ) where

import Test.Hspec
import Gratte.Command.Encryption
import qualified Data.ByteString.Char8 as BS

encryptionSpec :: IO ()
encryptionSpec = do
  let password = BS.replicate 32 'x'
      content  = "bla"
      padded   = "blaEND_CONTENT00"
  encrypted <- encrypt password content
  hspec $ do
    describe "pad" $
      it "pads content with control string and pad it with 0s" $ do
        pad content `shouldBe` padded
        rem (BS.length padded) 16 `shouldBe` 0
    describe "unpad" $
      it "unpads content" $
        unpad padded `shouldBe` Just content
    describe "encrypt and decrypt" $
      it "encrypts a bytestring using cbc" $
        decrypt password encrypted `shouldBe` Just content
