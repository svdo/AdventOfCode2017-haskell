module Day1Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day1

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parsing" $
    it "converts string to list of ints" $
      parseInput "123" `shouldBe` [1, 2, 3]
