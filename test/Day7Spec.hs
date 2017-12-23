module Day7Spec where

import Test.Hspec
import Day7

main :: IO ()
main = hspec spec

spec =
  describe "Day 7" $ do
    it "parses input" $
      parseInput "a (1)\nb (2) -> a, c\nc (3)\n" `shouldBe` [("a",1,[]), ("b",2,["a","c"]), ("c",3,[])]

    it "constructs set of all leafs" $
      leafsSet [("a",1,["b","c"]),("b",2,[]),("c",3,["b"])] `shouldBe` ["b","c"]

    it "constructs set of all names" $
      namesSet [("a",1,["b","c"]),("b",2,[]),("c",3,["b"])] `shouldBe` ["a","b","c"]

    it "determines the bottom" $
      bottom [("a",1,["b","c"]),("b",2,[]),("c",3,["b"])] `shouldBe` "a"

    it "can solve part 1" $
      bottom (parseInput day7Input) `shouldBe` "bsfpjtc"
