module Day12Spec (main, spec) where

import Test.Hspec
import Day12

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 12" $
    it "can test" $
      3 `shouldBe` 3
