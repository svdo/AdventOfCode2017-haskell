module Day9Spec (main, spec) where

import Test.Hspec
import Day9

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 9" $ do
    it "parses an empty group" $
      stream "{}" `shouldBe` ([], True, 1, [1])

    it "doesn't parse only an open brace" $
      stream "{" `shouldBe` ([], False, 0, [])

    it "parses nested empty group" $
      stream "{{}}" `shouldBe` ([], True, 2, [1,2])

    it "parses empty garbage" $
      stream "{<>}" `shouldBe` ([], True, 1, [1])

    it "recognizes empty garbage" $
      garbage "<>" 0 `shouldBe` ([], True, 0, [])

    it "recognizes non-empty garbage" $
      garbage "<a/b8*>" 0 `shouldBe` ([], True, 0, [])

    it "parses a group of garbage" $
      stream "{<asfasjkhfashflas>}" `shouldBe` ([], True, 1, [1])

    it "recognizes garbage with escaped gt" $
      garbage "<!>>" 0 `shouldBe` ([], True, 0, [])

    it "recognizes sample garbage" $ do
      garbage "<<<<>" 0 `shouldBe` ([], True, 0, [])
      garbage "<{!>}>" 0 `shouldBe` ([], True, 0, [])
      garbage "<!!>" 0 `shouldBe` ([], True, 0, [])
      garbage "<!!!>>" 0 `shouldBe` ([], True, 0, [])
      garbage "<{o\"i!a,<{i<a>" 0 `shouldBe` ([], True, 0, [])

    it "counts the number of groups in the samples" $ do
      stream "{{{}}}" `shouldBe` ([], True, 3, [1,2,3])
      stream "{{},{}}" `shouldBe` ([], True, 3, [1,2,2])
      stream "{{{},{},{{}}}}" `shouldBe` ([], True, 6, [1,2,3,3,3,4])
      stream "{<{},{},{{}}>}" `shouldBe` ([], True, 1, [1])
      stream "{<a>,<a>,<a>,<a>}" `shouldBe` ([], True, 1, [1])
      stream "{{<a>},{<a>},{<a>},{<a>}}" `shouldBe` ([], True, 5, [1,2,2,2,2])
      stream "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` ([], True, 2, [1,2])
