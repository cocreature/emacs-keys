module Main where

import Data.Char
import Data.Function
import EmacsKeys
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Text.XkbCommon.KeysymList

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = fmap (testGroup "tests") $ sequence [hspecTests, pure properties]

hspecTests :: IO TestTree
hspecTests = testSpec "hspec" $
  describe "parseEmacsKeys" $ do
    it "should parse M-Return" $
      parseEmacsKeys "M-Return" `shouldBe` Right ([Meta],[keysym_Return])
    it "should parse M-n" $
      parseEmacsKeys "M-n" `shouldBe` Right ([Meta],[keysym_n])
    it "should parse M-S-n" $
      parseEmacsKeys "M-S-N" `shouldBe` Right ([Meta,Shift],[keysym_N])
    it "should parse M-S-1" $
      parseEmacsKeys "M-S-1" `shouldBe` Right ([Meta,Shift],[keysym_1])

properties :: TestTree
properties =
  testGroup "properties"
            [QC.testProperty "shift converts to upper" $
             \char ->
               on (==)
                  (fmap snd)
                  (parseEmacsKeys
                     ("S-" ++
                      [char]))
                  (parseEmacsKeys [toUpper char]) ||
               char `elem`
               ['c','s','m']]
