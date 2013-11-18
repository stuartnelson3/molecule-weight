{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Peptide

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
  weight          <- getParam "weight"
  maybe (writeBS "must specify params in URL")
         writeBS peptideSequence

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
           writeBS param
