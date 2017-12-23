module Day3Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day3

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 3" $ do
    describe "part 1" $ do
      it "knows the number of items in ring 0" $
        elementsInRing 0 `shouldBe` 1

      it "knows the elements in ring 2" $
        elementsInRing 2 `shouldBe` 25

      it "knows the ring of 1" $
        ringOf 1 `shouldBe` 0

      it "knows the ring of 25" $
        ringOf 25 `shouldBe` 2

      it "knows the ring of 26" $
        ringOf 26 `shouldBe` 3

      it "knows the coordinate of 1" $
        coordinateOf 1 `shouldBe` (0,0)

      it "knows the coordinate of 26" $
        coordinateOf 26 `shouldBe` (3,2)

      it "knows the coordinate of 17" $
        coordinateOf 17 `shouldBe` (-2,-2)

      it "knows the position in ring at start" $
        moveInRing 3 0 `shouldBe` (3, 2)

      it "knows the position in ring moving up" $ do
        moveInRing 3 1 `shouldBe` (3, 1)
        moveInRing 3 2 `shouldBe` (3, 0)
        moveInRing 3 3 `shouldBe` (3, -1)
        moveInRing 3 4 `shouldBe` (3, -2)
        moveInRing 3 5 `shouldBe` (3, -3)

      it "knows the position in ring moving left" $ do
        moveInRing 3 6 `shouldBe` (2, -3)
        moveInRing 3 7 `shouldBe` (1, -3)
        moveInRing 3 8 `shouldBe` (0, -3)
        moveInRing 3 9 `shouldBe` (-1, -3)
        moveInRing 3 10 `shouldBe` (-2, -3)
        moveInRing 3 11 `shouldBe` (-3, -3)

      it "knows the position in ring moving down" $ do
        moveInRing 3 12 `shouldBe` (-3, -2)
        moveInRing 3 13 `shouldBe` (-3, -1)
        moveInRing 3 14 `shouldBe` (-3, 0)
        moveInRing 3 15 `shouldBe` (-3, 1)
        moveInRing 3 16 `shouldBe` (-3, 2)
        moveInRing 3 17 `shouldBe` (-3, 3)

      it "knows the position in ring moving right" $ do
        moveInRing 3 18 `shouldBe` (-2, 3)
        moveInRing 3 19 `shouldBe` (-1, 3)
        moveInRing 3 20 `shouldBe` (0, 3)
        moveInRing 3 21 `shouldBe` (1, 3)
        moveInRing 3 22 `shouldBe` (2, 3)
        moveInRing 3 23 `shouldBe` (3, 3)

      it "knows the manhattan distance" $ do
        manhattanDistance 1 `shouldBe` 0
        manhattanDistance 2 `shouldBe` 1
        manhattanDistance 12 `shouldBe` 3
        manhattanDistance 23 `shouldBe` 2
        manhattanDistance 1024 `shouldBe` 31

      it "can solve the puzzle" $
        manhattanDistance day3Input `shouldBe` 438

    describe "part 2" $ do
      it "starts with 1" $
        grid 1 `shouldBe` [((0,0), 1)]

      it "index 2 is also 1" $
        grid 2 `shouldBe` [((1,0), 1), ((0,0), 1)]

      it "index 3 is 2" $
        grid 3 `shouldBe` [((1,-1), 2), ((1,0), 1), ((0,0), 1)]

      it "index 4 is 4" $
        grid 4 `shouldBe` [((0,-1), 4), ((1,-1), 2), ((1,0), 1), ((0,0), 1)]

      it "index 23 is 806" $
        snd(head(grid 23)) `shouldBe` 806

      it "solves part 2" $
        day3part2 `shouldBe` 266330

-- observation:
-- square is built the following way:
-- 1 right 1 up
-- 2 left 2 down
-- 3 right 3 up
-- 4 left 4 down
-- 5 right 5 up
-- etc
