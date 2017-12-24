module Day11Spec (main, spec) where

import Test.Hspec
import Day11

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 11" $
    it "can test" $
      3 `shouldBe` 3
