module Day14Spec (main, spec) where

import Test.Hspec
import Day10
import Day14

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 14" $
    it "solves part 1" $ do
      pendingWith "Long running test; disabled for performance"
      numOnes (hexStringToBinaryString day14Input) `shouldBe` 8226

