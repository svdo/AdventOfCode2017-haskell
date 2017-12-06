module Day2Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day2

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "day2" $ do
    it "can determine difference between max and min of a row" $
      rowDiff [1,9,5] `shouldBe` 8

    it "can determine a sample checksum" $
      checksum "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8" `shouldBe` 18

    it "can parse a spreadsheet string" $ do
      let sheetString = "790\t99\t345\n302\t463\t59\n899\t962\t77\n"
      let expected = [[790,99,345],[302,463,59],[899,962,77]]
      parseSpreadsheet sheetString `shouldBe` expected

    it "can solve part 1" $
      checksum spreadsheet `shouldBe` 46402

    describe "part 2" $ do
      it "can generate pairs of elements in a list" $
        pairs [1,2,3,4] `shouldBe` [(1,2), (1,3), (1,4),
                                    (2,1), (2,3), (2,4),
                                    (3,1), (3,2), (3,4),
                                    (4,1), (4,2), (4,3)]

      it "can compute div of list of pairs" $
        divs [(2,2), (4,2), (5,2)] `shouldBe` [1,2,0]

      it "can compute answer of sample data" $
        computeSumOfEvenlyDivisible [[5, 9, 2, 8],
                                    [9, 4, 7, 3],
                                    [3, 8, 6, 5]] `shouldBe` 9

      it "can solve part 2" $
        sumOfEvenlyDivisible spreadsheet `shouldBe` 265
