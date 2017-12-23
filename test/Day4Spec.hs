module Day4Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 4" $ do
    context "given a list of words" $ do
      it "has no duplicates" $
        hasDuplicates ["aa", "bb", "cc"] `shouldBe` False

      it "has duplicates" $
        hasDuplicates ["aa", "bb", "cc", "dd", "aa"] `shouldBe` True

      it "counts valid phrases" $
        countValid [["aa", "bb", "cc"],
                    ["aa", "bb", "cc", "dd", "aa"],
                    ["aa", "bb", "cc"]] `shouldBe` 2

      it "knows the solution of part 1" $
        countValid (map words day4Input) `shouldBe` 477

    describe "part 2" $ do
      it "checks if a list of words contains anagrams of a word" $ do
        hasAnagram "abcde" ["xyz", "ecdab"] `shouldBe` True
        hasAnagram "bcde" ["xyz", "cdab"] `shouldBe` False

      it "checks is a list of words has anagrams" $ do
        hasAnagrams ["abcde", "xyz", "ecdab"] `shouldBe` True
        hasAnagrams ["bcde", "xyz", "cdab"] `shouldBe` False

      it "solves part 2" $
        countValid2 (map words day4Input) `shouldBe` 167
