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

    it "constructs a simple tower" $
      constructTower [("a", 1, [])] `shouldBe` MkTower "a" 1 []

    it "constructs tower with one level of subtowers" $
      constructTower [("a",1,["b","c"]),("b",2,[]),("c",2,[])] `shouldBe`
        MkTower "a" 5 [
          MkTower "b" 2 [],
          MkTower "c" 2 []
        ]

    it "gives the fixed weight of the example" $
      fixedWeight sample `shouldBe` 60

    it "solves part 2" $ do
      pendingWith "Long running test; disabled for performance"
      fixedWeight day7Input `shouldBe` 529

    it "constructs the tower of the example" $
      constructTower (parseInput sample) `shouldBe`
        MkTower "tknk" (-60) [
          MkTower "ugml" 251 [
            MkTower "gyxo" 61 [],
            MkTower "ebii" 61 [],
            MkTower "jptl" 61 []
          ],
          MkTower "padx" 243 [
            MkTower "pbga" 66 [],
            MkTower "havc" 66 [],
            MkTower "qoyq" 66 []
          ],
          MkTower "fwft" 243 [
            MkTower "ktlj" 57 [],
            MkTower "cntj" 57 [],
            MkTower "xhth" 57 []
          ]
        ]


sample = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\n qoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
