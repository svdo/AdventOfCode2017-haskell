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
      garbage "<>" `shouldBe` ([], True, 0)

    it "recognizes non-empty garbage" $
      garbage "<a/b8*>" `shouldBe` ([], True, 5)

    it "parses a group of garbage" $
      stream "{<asfasjkhfashflas>}" `shouldBe` ([], True, 1, [1])

    it "recognizes garbage with escaped gt" $
      garbage "<!>>" `shouldBe` ([], True, 0)

    it "recognizes sample garbage" $ do
      garbage "<<<<>" `shouldBe` ([], True, 3)
      garbage "<{!>}>" `shouldBe` ([], True, 2)
      garbage "<!!>" `shouldBe` ([], True, 0)
      garbage "<!!!>>" `shouldBe` ([], True, 0)
      garbage "<{o\"i!a,<{i<a>" `shouldBe` ([], True, 10)

    it "counts the number of groups in the samples" $ do
      stream "{{{}}}" `shouldBe` ([], True, 3, [1,2,3])
      stream "{{},{}}" `shouldBe` ([], True, 3, [1,2,2])
      stream "{{{},{},{{}}}}" `shouldBe` ([], True, 6, [1,2,3,3,3,4])
      stream "{<{},{},{{}}>}" `shouldBe` ([], True, 1, [1])
      stream "{<a>,<a>,<a>,<a>}" `shouldBe` ([], True, 1, [1])
      stream "{{<a>},{<a>},{<a>},{<a>}}" `shouldBe` ([], True, 5, [1,2,2,2,2])
      stream "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` ([], True, 2, [1,2])

    it "determines the score of the samples" $ do
      score "{}" `shouldBe` 1
      score "{{{}}}" `shouldBe` 6
      score "{{},{}}" `shouldBe` 5
      score "{{{},{},{{}}}}" `shouldBe` 16
      score "{<a>,<a>,<a>,<a>}" `shouldBe` 1
      score "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
      score "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
      score "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3

    it "solves day 9" $
      score day9Input `shouldBe` 17537
