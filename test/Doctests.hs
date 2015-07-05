module Main where

import Test.DocTest

main =
  doctest ["-isrc"
          ,"src/EmacsKeys.hs"
          ,"src/EmacsKeys/Parser.hs"
          ,"src/EmacsKeys/TH.hs"]
