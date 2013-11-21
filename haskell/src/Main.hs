{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Extras.JSON
import           Peptide
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("possible-matches/:peptide_sequence/:weight", possibleMatches)
          ] <|>
    dir "static" (serveDirectory ".")

possibleMatches :: Snap ()
possibleMatches = do
  Just peptideSequence <- getParam "peptide_sequence"
  Just foundWeight     <- getParam "weight"
  writeJSON $ calculationResults (read $ C8.unpack foundWeight) peptideSequence

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
           writeBS param
