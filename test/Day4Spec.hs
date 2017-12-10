module Day4Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 4" $
    context "given a list of words" $ do
      it "has no duplicates" $
        duplicates ["aa", "bb", "cc"] `shouldBe` False

      it "has duplicates" $
        duplicates ["aa", "bb", "cc", "dd", "aa"] `shouldBe` True

      it "counts valid phrases" $
        countValid [["aa", "bb", "cc"],
                    ["aa", "bb", "cc", "dd", "aa"],
                    ["aa", "bb", "cc"]] `shouldBe` 2

      it "knows the solution of part 1" $
        countValid (map words day4Input) `shouldBe` 477
