module Day10Spec (main, spec) where

import Test.Hspec
import Day10

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 10" $
    it "can test" $
      3 `shouldBe` 4
