module Day5Spec (main, spec) where

import Test.Hspec
import Day5

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 5" $ do
    it "does one step in simplest case" $
      stepsToExit [1] `shouldBe` 1

    it "can update the list" $ do
      update 0 [1, 2, 3] `shouldBe` [2, 2, 3]
      update 1 [1, 2, 3] `shouldBe` [1, 3, 3]
      update 2 [1, 2, 3] `shouldBe` [1, 2, 4]

    it "can jump through the example" $
      jump 0 0 [0,3,0,1,-3] `shouldBe` [5]

    it "can jump out on the left" $
      jump 0 0 [-1] `shouldBe` [1]

    it "returns the number of steps" $
      stepsToExit [0,3,0,1,-3] `shouldBe` 5

    it "can solve the puzzle" $
      stepsToExit day5Input `shouldBe` 364539
