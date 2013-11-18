module Peptide (calculationResults) where

import Text.RegexPR
import Data.Char
import Data.List

aminoAcidWeight :: [Char] -> Float
aminoAcidWeight "a"    = 71.04
aminoAcidWeight "c"    = 103.01
aminoAcidWeight "d"    = 115.03
aminoAcidWeight "e"    = 129.04
aminoAcidWeight "f"    = 147.07
aminoAcidWeight "g"    = 57.02
aminoAcidWeight "h"    = 137.06
aminoAcidWeight "i"    = 113.08
aminoAcidWeight "k"    = 128.09
aminoAcidWeight "l"    = 113.08
aminoAcidWeight "m"    = 131.04
aminoAcidWeight "n"    = 114.04
aminoAcidWeight "p"    = 97.05
aminoAcidWeight "q"    = 128.06
aminoAcidWeight "r"    = 156.1
aminoAcidWeight "s"    = 87.03
aminoAcidWeight "t"    = 101.05
aminoAcidWeight "v"    = 99.07
aminoAcidWeight "w"    = 186.08
aminoAcidWeight "y"    = 163.06
aminoAcidWeight "(3a)" = 85.06
aminoAcidWeight "(3c)" = 117.03
aminoAcidWeight "(3d)" = 129.05
aminoAcidWeight "(3e)" = 143.06
aminoAcidWeight "(3f)" = 161.09
aminoAcidWeight "(3g)" = 71.04
aminoAcidWeight "(3h)" = 151.08
aminoAcidWeight "(3i)" = 127.1
aminoAcidWeight "(3k)" = 142.11
aminoAcidWeight "(3l)" = 127.1
aminoAcidWeight "(3n)" = 128.06
aminoAcidWeight "(3p)" = 111.07
aminoAcidWeight "(3q)" = 142.07
aminoAcidWeight "(3r)" = 170.12
aminoAcidWeight "(3s)" = 101.05
aminoAcidWeight "(3t)" = 115.07
aminoAcidWeight "(3v)" = 113.09
aminoAcidWeight "(3w)" = 200.1
aminoAcidWeight "(3y)" = 117.08
aminoAcidWeight "x"    = 111.07
aminoAcidWeight "z"    = 112.06
aminoAcidWeight "u"    = 85.05

lowerString :: [Char] -> [Char]
lowerString string = [ toLower x | x <- string ]

parsePeptide :: [Char] -> [[Char]]
parsePeptide sequence = [ match | ((match,_),_) <- regexSequence ]
                        where regexSequence = gmatchRegexPR "\\([0-9][a-z]\\)|[a-z]" lowerSequence
                              lowerSequence = lowerString sequence

calculateWeight :: [[Char]] -> Float
calculateWeight sequence = 19 + sum aminoAcids
                           where aminoAcids = [ aminoAcidWeight x | x <- sequence ]

possibleFragments :: [[Char]] -> [[[Char]]]
possibleFragments sequence = concat [ fragmentsByLength x sequence | x <- [1..length sequence] ]

fragmentsByLength :: Int -> [[Char]] -> [[[Char]]]
fragmentsByLength 1   (x:xs) = [ x:[] | x <- (x:xs) ]
fragmentsByLength len (x:xs)
  | length (x:xs) >= len = (take len (x:xs)):(fragmentsByLength len xs)
  | otherwise            = []

weightWithinTolerance :: Float -> [[Char]] -> Bool
weightWithinTolerance weight fragment = fragmentWeight + 5 > weight && fragmentWeight - 5 < weight
                                        where fragmentWeight = calculateWeight fragment

possibleMatches :: Float -> [[[Char]]] -> [[[Char]]]
possibleMatches weight fragments = [ fragment | fragment <- fragments,
                                     weightWithinTolerance weight fragment ]

humanReadable :: [[Char]] -> [Char]
humanReadable fragment = [toUpper x | x <- intercalate "" fragment]

calculationResults :: Float -> [Char] -> [([Char], Float)]
calculationResults weight sequence = results
  where pf = possibleFragments $ parsePeptide sequence
        pm = possibleMatches weight pf
        humanPM = map humanReadable pm
        pw = map calculateWeight pm
        results = zip humanPM pw
