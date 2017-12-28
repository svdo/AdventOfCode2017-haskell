module Day13Spec (main, spec) where

import Test.Hspec
import Day13
import Data.List

main :: IO ()
main = hspec spec

exampleInput = [(0,3), (1,2), (4,4), (6,4)]
exampleFirewall = firewall exampleInput

spec :: Spec
spec =
  describe "Day 13" $ do
    it "constructs a list of depths from input" $
      firewall [(0,3),(1,2),(4,4),(6,4)] `shouldBe` [3,2,0,0,4,0,4]

    it "checks caught" $ do
      caughtAt 0 exampleFirewall (replicate (length exampleFirewall) 0) `shouldBe` True
      caughtAt 0 exampleFirewall (replicate (length exampleFirewall) 1) `shouldBe` False
      caughtAt 2 exampleFirewall (replicate (length exampleFirewall) 0) `shouldBe` False

    it "updates the scanner" $ do
      updateScanners exampleFirewall [0,0,0,0,0,0,0] `shouldBe` [1,1,0,0,1,0,1]
      updateScanners exampleFirewall [1,1,0,0,1,0,1] `shouldBe` [2,0,0,0,2,0,2]
      updateScanners exampleFirewall [2,0,0,0,2,0,2] `shouldBe` [-1,1,0,0,3,0,3]
      updateScanners exampleFirewall [-1,1,0,0,3,0,3] `shouldBe` [0,0,0,0,-2,0,-2]

    it "determines when you're caught" $
      passage exampleFirewall `shouldBe` [0, 6]

    it "determines the severity" $
      severity exampleFirewall [0,6] `shouldBe` 24

    it "solves part 1" $
      severity day13Firewall (passage day13Firewall) `shouldBe` 2160

    it "determines min delay for example firewall" $
      passageWithoutGettingCaught exampleFirewall 0 `shouldBe` 10

    it "solves part 2" $
--      passageWithoutGettingCaught day13Firewall 0 `shouldBe` 3907470
      day13b day13Input `shouldBe` 3907470


    it "tries passage" $
      tryPassage 0 exampleFirewall [0,0,0,0,0,0,0] `shouldBe` False

    it "updates scanners n times" $ do
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 1 `shouldBe` [1,1,0,0,1,0,1]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 2 `shouldBe` [2,0,0,0,2,0,2]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 3 `shouldBe` [-1,1,0,0,3,0,3]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 4 `shouldBe` [0,0,0,0,-2,0,-2]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 5 `shouldBe` [1,1,0,0,-1,0,-1]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 6 `shouldBe` [2,0,0,0,0,0,0]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 7 `shouldBe` [-1,1,0,0,1,0,1]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 8 `shouldBe` [0,0,0,0,2,0,2]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 9 `shouldBe` [1,1,0,0,3,0,3]
      updateScanners' exampleFirewall [0,0,0,0,0,0,0] 10 `shouldBe` [2,0,0,0,-2,0,-2]
