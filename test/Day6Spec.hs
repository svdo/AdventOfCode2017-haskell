module Day6Spec where

import Test.Hspec
import Day6

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 6" $ do
    it "updates a list of numbers" $
      update [0,2,7,0] `shouldBe` [2,4,1,2]

    it "can set an element to zero" $ do
      setZero 0 [1,2,3] `shouldBe` [0,2,3]
      setZero 1 [1,2,3] `shouldBe` [1,0,3]
      setZero 2 [1,2,3] `shouldBe` [1,2,0]

    it "distributes values" $ do
      distribute 3 [2,4,6] 0 `shouldBe` [3,5,7]
      distribute 3 [2,4,6] 1 `shouldBe` [3,5,7]
      distribute 4 [10,20,30] 1 `shouldBe` [11,22,31]

    it "knows amount of steps unique" $
      stepsUnique [0,2,7,0] `shouldBe` (5, 4)

    it "knows the answer to part 1" $
      stepsUnique day6Input `shouldBe` (4074, 2793)
