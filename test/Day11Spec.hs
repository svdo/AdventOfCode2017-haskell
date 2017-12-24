module Day11Spec (main, spec) where

import Test.Hspec
import Day11

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 11" $ do
    it "starts at origin" $
      followDirections [] `shouldBe` (0,0)

    it "moves in any direction" $ do
      followDirectionsFrom (0, 0) [N] `shouldBe` (0, 1)
      followDirectionsFrom (0, 0) [S] `shouldBe` (0, -1)
      followDirectionsFrom (0, 0) [NE] `shouldBe` (1, 0)
      followDirectionsFrom (0, 0) [SW] `shouldBe` (-1, 0)
      followDirectionsFrom (0, 0) [NW] `shouldBe` (-1, 1)
      followDirectionsFrom (0, 0) [SE] `shouldBe` (1, -1)

    it "moves multiple steps" $
      followDirectionsFrom (0, 0) [NW,NW,NW] `shouldBe` (-3, 3)

    it "can parse an empty directions string" $
      parseDirections "" `shouldBe` []

    it "can parse a directions string with one element" $ do
      parseDirections "n" `shouldBe` [N]
      parseDirections "s" `shouldBe` [S]
      parseDirections "ne" `shouldBe` [NE]
      parseDirections "sw" `shouldBe` [SW]
      parseDirections "nw" `shouldBe` [NW]
      parseDirections "se" `shouldBe` [SE]

    it "can parse multiple directions" $
      parseDirections "n,s,ne,sw,nw,se" `shouldBe` [N,S,NE,SW,NW,SE]

    it "follows directions of input" $
      followDirections (parseDirections day11Input) `shouldBe` (-559,698)

    it "knows the distance of the input directions" $
      distance (parseDirections day11Input) `shouldBe` 698
