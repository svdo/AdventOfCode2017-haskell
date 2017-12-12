module Day9Spec (main, spec) where

import Test.Hspec
import Day9

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 9" $ do
    it "parses an empty group" $
      stream "{}" `shouldBe` ([], True, 1)

    it "doesn't parse only an open brace" $
      stream "{" `shouldBe` ([], False, 0)

    it "parses nested empty group" $
      stream "{{}}" `shouldBe` ([], True, 2)

--    it "parses a group of garbage" $
--      stream "{<asfasjkhfashflas>}" `shouldBe` ([], True)
