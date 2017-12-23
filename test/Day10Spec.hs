module Day10Spec (main, spec) where

import Test.Hspec
import Day10

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 10" $ do
    it "performs reverse" $ do
      performReverse 0 [0,1,2,3,4] [2,1,0] `shouldBe` [2,1,0,3,4]
      performReverse 3 [2,1,0,3,4] [1,2,4,3] `shouldBe` [4,3,0,1,2]

    it "does example step 1" $
      step ([0..4], 0, 0, [3, 4, 1, 5]) `shouldBe` ([2,1,0,3,4], 3, 1, [4,1,5])

    it "does example step 2" $
      step ([2,1,0,3,4], 3, 1, [4,1,5]) `shouldBe` ([4,3,0,1,2], 3, 2, [1,5])

    it "does example step 3" $
      step ([4,3,0,1,2], 3, 2, [1,5]) `shouldBe` ([4,3,0,1,2], 1, 3, [5])

    it "does example step 4" $
      step ([4,3,0,1,2], 1, 3, [5]) `shouldBe` ([3,4,2,1,0], 4, 4, [])

    it "does all steps" $
      oneRound ([0..4], 0, 0, [3, 4, 1, 5]) `shouldBe` ([3,4,2,1,0], 4, 4, [])

    it "computes example hash" $
      hash1 5 [3,4,1,5] `shouldBe` 12

    it "solves part 1" $
      hash1 256 day10Input `shouldBe` 2928

