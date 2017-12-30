module Day15Spec (main, spec) where

import Test.Hspec
import Day15

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day 15" $ do
    describe "part 1" $ do
      it "generates example values" $ do
        generateNext (65,8921) `shouldBe` (1092455,430625591)
        generateNext (1092455,430625591) `shouldBe` (1181022009,1233683848)
        generateNext (1181022009,1233683848) `shouldBe` (245556042,1431495498)
        generateNext (245556042,1431495498) `shouldBe` (1744312007,137874439)
        generateNext (1744312007,137874439) `shouldBe` (1352636452,285222916)

      it "generates multiple" $
        generate ((65,8921), 0) 5 `shouldBe` ((1352636452,285222916), 1)

      it "solves example" $ do
        pendingWith "Long running test; disabled for performance"
        snd (generate ((65,8921), 0) 40000000) `shouldBe` 588

      it "solves part 1" $ do
        pendingWith "Long running test; disabled for performance"
        snd (generate (day15Input, 0) 40000000) `shouldBe` 569

    describe "part 2" $ do
      it "generates example values" $ do
        generateNext2a 65 `shouldBe` 1352636452
        generateNext2a 1352636452 `shouldBe` 1992081072
        generateNext2a 1992081072 `shouldBe` 530830436
        generateNext2a 530830436 `shouldBe` 1980017072
        generateNext2a 1980017072 `shouldBe` 740335192

      it "solves example" $ do
        pendingWith "Long running test; disabled for performance"
        snd (generate2 ((65,8921), 0) 5000000) `shouldBe` 309

      it "solves part 2" $ do
        pendingWith "Long running test; disabled for performance"
        snd (generate2 (day15Input, 0) 5000000) `shouldBe` 298
