import Text.RegexPR
import Data.Char

aminoAcidWeight :: [Char] -> Float
aminoAcidWeight x | x == "a"    = 71.04
                  | x == "c"    = 103.01
                  | x == "d"    = 115.03
                  | x == "e"    = 129.04
                  | x == "f"    = 147.07
                  | x == "g"    = 57.02
                  | x == "h"    = 137.06
                  | x == "i"    = 113.08
                  | x == "k"    = 128.09
                  | x == "l"    = 113.08
                  | x == "m"    = 131.04
                  | x == "n"    = 114.04
                  | x == "p"    = 97.05
                  | x == "q"    = 128.06
                  | x == "r"    = 156.1
                  | x == "s"    = 87.03
                  | x == "t"    = 101.05
                  | x == "v"    = 99.07
                  | x == "w"    = 186.08
                  | x == "y"    = 163.06
                  | x == "(3a)" = 85.06
                  | x == "(3c)" = 117.03
                  | x == "(3d)" = 129.05
                  | x == "(3e)" = 143.06
                  | x == "(3f)" = 161.09
                  | x == "(3g)" = 71.04
                  | x == "(3h)" = 151.08
                  | x == "(3i)" = 127.1
                  | x == "(3k)" = 142.11
                  | x == "(3l)" = 127.1
                  | x == "(3n)" = 128.06
                  | x == "(3p)" = 111.07
                  | x == "(3q)" = 142.07
                  | x == "(3r)" = 170.12
                  | x == "(3s)" = 101.05
                  | x == "(3t)" = 115.07
                  | x == "(3v)" = 113.09
                  | x == "(3w)" = 200.1
                  | x == "(3y)" = 117.08
                  | x == "x"    = 111.07
                  | x == "z"    = 112.06
                  | x == "u"    = 85.05

lowerString :: [Char] -> [Char]
lowerString string = [ toLower x | x <- string ]

parsePeptide :: [Char] -> [[Char]]
parsePeptide sequence = [ match | ((match,_),_) <- regexSequence ]
                        where regexSequence = gmatchRegexPR "\\([0-9][a-z]\\)|[a-z]" lowerSequence
                              lowerSequence = lowerString sequence

calculateWeight :: [[Char]] -> Float
calculateWeight sequence = sum aminoAcids
                           where aminoAcids = [ aminoAcidWeight x | x <- sequence ]
main = do
  printf "Enter your sequence:"
  sequence <- getLine
  print $ calculateWeight $ parsePeptide sequence
