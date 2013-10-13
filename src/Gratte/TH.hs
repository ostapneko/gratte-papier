{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Gratte.TH
  ( mkDocField
  , DocField(..)
  ) where

import Control.Monad

import Data.Aeson
import Data.Maybe
import Language.Haskell.TH
import qualified Data.Text as T

mkDocField :: String -> Q [Dec]
mkDocField field = do
  toJSONClass   <- [t|ToJSON|]
  fromJSONClass <- [t|FromJSON|]
  let fieldN = mkFieldN field
      fieldC = mkFieldC fieldN
      fieldT = ConT fieldN
      fieldTypeD = NewtypeD [] fieldN [] fieldC [mkName "Show"]
  toJSONFunD       <- mkToJSONFunD fieldN
  parseJSONFunD    <- mkParseJSONFunD fieldN
  fieldToTextFunDs <- mkFieldToTextFunDs field
  let toJSONInsD   = InstanceD [] (AppT toJSONClass fieldT) [toJSONFunD]
  let fromJSONInsD = InstanceD [] (AppT fromJSONClass fieldT) [parseJSONFunD]
  return $ [fieldTypeD, toJSONInsD, fromJSONInsD] ++ fieldToStringFunDs field ++ fieldToTextFunDs

newtype DocField = DocField String

mkFieldN :: String -> Name
mkFieldN fieldN = mkName $ "Document" ++ fieldN

mkFieldC :: Name -> Con
mkFieldC name = NormalC name [(NotStrict, stringT)]

stringT :: Type
stringT = ConT $ mkName "String"

mkToJSONFunD :: Name -> Q Dec
mkToJSONFunD fieldN = do
  packF     <- [| T.pack |]
  toJSONFun <- [| toJSON |]
  let (fPat, fVar) = mkPandV "f"
      packedField  = AppE packF fVar                      -- T.pack f
      body         = NormalB $ AppE toJSONFun packedField -- toJSON $ T.pack f
      clause'      = Clause [ConP fieldN [fPat]] body []  -- toJSON (DocumentHash f) = body
  return $ FunD (mkName "toJSON") [clause']

mkParseJSONFunD :: Name -> Q Dec
mkParseJSONFunD fieldN = do
  unpackF  <- [| T.unpack |]
  stringCN <- lookupValueName "String"
  returnF  <- [| return |]
  mzeroF   <- [| mzero |]
  let (fPat, fVar) = mkPandV "f"
      stringP       = ConP (fromJust stringCN) [fPat]    -- String f
      unpackedField = AppE unpackF fVar                  -- T.unpack f
      fieldCE       = ConE fieldN                        -- DocumentHash
      field         = AppE fieldCE unpackedField         -- DocumentHash $ T.unpack f
      happyBody     = NormalB $ AppE returnF field       -- return $ DocumentHash $ T.unpack f
      happyClause   = Clause [stringP] happyBody []      -- parseJSON (String f) = body
      unhappyClause = Clause [WildP] (NormalB mzeroF) [] -- parseJSON _ = mzero
  return $ FunD (mkName "parseJSON") [happyClause, unhappyClause]

-- | Generate the functions docHashToString :: DocumentHash -> String
fieldToStringFunDs :: String -> [Dec]
fieldToStringFunDs field =
  let funName      = mkName $ "doc" ++ field ++ "ToString"
      fieldN       = mkFieldN field
      fieldT       = ConT fieldN
      (fPat, fVar) = mkPandV "f"
      clause'      = Clause [ConP fieldN [fPat]] (NormalB fVar) []    -- DocumentHash f = f
      sigD'        = SigD funName (AppT (AppT ArrowT fieldT) stringT) -- DocumentHash -> String
  in [FunD funName [clause'], sigD']

-- | Generate the functions docHashToText :: DocumentHash -> T.Text
mkFieldToTextFunDs :: String -> Q [Dec]
mkFieldToTextFunDs field = do
  packF <- [| T.pack |]
  textT <- [t|T.Text |]
  let funName      = mkName $ "doc" ++ field ++ "ToText"
      fieldN       = mkFieldN field
      fieldT       = ConT fieldN
      (fPat, fVar) = mkPandV "f"
      clause'      = Clause [ConP fieldN [fPat]] (NormalB (AppE packF fVar)) [] -- DocumentHash f = T.pack f
      sigD'        = SigD funName (AppT (AppT ArrowT fieldT) textT)             -- DocumentHash -> T.Text
  return [FunD funName [clause'], sigD']

mkPandV :: String -> (Pat, Exp)
mkPandV x = let name = mkName x
            in (VarP name, VarE name)
