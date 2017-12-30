module Day14Spec (main, spec) where

import Test.Hspec
import Day10
import Day14
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 14" $ do
    it "solves part 1" $
      numOnes (hexStringToBinaryString day14Input) `shouldBe` 8226

    it "constructs node list from binary arrays" $ do
      binaryStringsToNodeList ["10","00"] `shouldBe` [(0,[])]
      binaryStringsToNodeList ["11","00"] `shouldBe` [(0,[1]),(1,[0])]
      binaryStringsToNodeList ["10","10"] `shouldBe` [(0,[2]),(2,[0])]
      binaryStringsToNodeList ["10","01"] `shouldBe` [(0,[]),(3,[])]

--    it "finds groups" $ do
--      findGroups ["10","00"] `shouldBe` [[0]]
--      findGroups ["11","00"] `shouldBe` [[0,1]]

    it "converts to ints" $
      toInts ["10","00"] `shouldBe` [[1,0],[0,0]]

    it "can replace at coordinate" $ do
      replace (1,2) 9 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` [[0,1,2],[3,4,9],[6,7,8]]
      replace (0,0) 9 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` [[9,1,2],[3,4,5],[6,7,8]]
      replace (2,2) 9 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` [[0,1,2],[3,4,5],[6,7,9]]

    it "finds the first entry with value" $ do
      findFirst 0 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` Just (0,0)
      findFirst 1 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` Just (0,1)
      findFirst 8 [[0,1,2],[3,4,5],[6,7,8]] `shouldBe` Just (2,2)

    it "follows the ones" $ do
      markRegion [[1,1,0],[0,1,0],[1,0,0]] 2 `shouldBe` [[2,2,0],[0,2,0],[1,0,0]]
      markRegion [[2,2,0],[0,2,0],[1,0,0]] 3 `shouldBe` [[2,2,0],[0,2,0],[3,0,0]]

    it "marks all regions" $
      markAllRegions [[1,1,0],[0,1,0],[1,0,0]] `shouldBe` [[2,2,0],[0,2,0],[3,0,0]]

    it "solves part 2" $
      countRegions (toInts (hexStringToBinaryString day14Input)) `shouldBe` 1128
