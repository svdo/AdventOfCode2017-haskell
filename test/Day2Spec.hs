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

    it "can parse a spreadsheet string" $
      parseSpreadsheet sheetString `shouldBe` expected
        where sheetString = "790\t99\t345\n302\t463\t59\n899\t962\t77\n"
              expected = [[790,99,345],[302,463,59],[899,962,77]]


