module Main where

import Data.Char
import EmacsKeys
import Test.Hspec
import Test.QuickCheck
import Text.XkbCommon.KeysymList

main :: IO ()
main =
  hspec $ do
    describe "parseEmacsKeys" $ do
      it "should parse M-Return" $
        parseEmacsKeys "M-Return" `shouldBe` Right ([Meta], [keysym_Return])
      it "should parse M-n" $
        parseEmacsKeys "M-n" `shouldBe` Right ([Meta], [keysym_n])
      it "should parse M-S-n" $
        parseEmacsKeys "M-S-N" `shouldBe` Right ([Meta, Shift], [keysym_N])
      it "should parse M-S-1" $
        parseEmacsKeys "M-S-1" `shouldBe` Right ([Meta, Shift], [keysym_1])
      it "converts to upper when shift is pressed" $
        property $ \char ->
          fmap snd (parseEmacsKeys ("S-" ++ [char])) ==
          fmap snd (parseEmacsKeys [toUpper char]) ||
          char `elem` ['c', 's', 'm']
