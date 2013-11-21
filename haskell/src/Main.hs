{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Extras.JSON
import           Peptide
import           Data.Maybe
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
  peptideSequence <- getParam "peptide_sequence"
  foundWeight     <- getParam "weight"
  let results = calculationResults (read . C8.unpack $ (fromJust foundWeight)) (fromJust peptideSequence)
  maybe (writeBS "must specify params in URL")
         writeJSON $ Just (results)

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
           writeBS param
