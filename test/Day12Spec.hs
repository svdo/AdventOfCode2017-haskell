module Day12Spec (main, spec) where

import Test.Hspec
import Day12
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 12" $ do
    it "parses input line" $
      parseNode "0 <-> 2, 5, 7" `shouldBe` (0, [2,5,7])

    it "parses multiple lines of input" $
      parseNodes "0 <-> 2, 5, 7\n2 <-> 0\n" `shouldBe` [(0, [2,5,7]), (2, [0])]

    it "finds a node in a list" $
      findNode 0 [(0, [2,5,7]), (2, [0])] `shouldBe` (0, [2,5,7])

    it "extracts a node from a list" $
      extractNode 0 [(0, [2,5,7]), (2, [0])] `shouldBe` ((0, [2,5,7]), [(2, [0])])

    describe "part1" $ do
      it "handles empty list" $
        reachable 0 [] [] `shouldBe` []

      it "handles list of one" $
        reachable 0 [(0, [])] [] `shouldBe` []

      it "includes a reachable node" $
        reachable 0 [(0, [1]), (1, [0])] [] `shouldBe` [1,0]

      it "includes an indirectly reachable node" $
        sort (reachable 0 [(0, [1]), (1, [0,2]), (2, [1])] []) `shouldBe` [0,1,2]

      it "solves part 1" $
        length (reachable 0 (parseNodes day12Input) []) `shouldBe` 128

    describe "part 2" $ do
      it "solves part 2" $
        length (groups (map fst parsedDay12Input) parsedDay12Input) `shouldBe` 209

      it "finds a single group" $
        groups [0] [(0,[])] `shouldBe` [[0]]
