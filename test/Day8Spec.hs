module Day8Spec (main,spec) where

import Test.Hspec
import Day8

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "day 8" $ do
    it "parses a line" $ do
      parseInstruction "a inc 1 if b < 5" `shouldBe` ("a", Inc, 1, "b", Lt, 5)
      parseInstruction "b dec -2 if a >= -2" `shouldBe` ("b", Dec, -2, "a", Gte, -2)

    it "can execute an instruction for register that was not yet known" $ do
      execute [] ("a", Inc, 1, "b", Lt, 5) `shouldBe` [("a", 1)]
      execute [] ("a", Dec, 1, "b", Lt, 5) `shouldBe` [("a", -1)]

    it "can execute an instruction for register that was already known" $ do
      execute [("a", 3)] ("a", Inc, 1, "b", Lt, 5) `shouldBe` [("a", 4)]
      execute [("a", 3)] ("a", Dec, 1, "b", Lt, 5) `shouldBe` [("a", 2)]

    it "leaves other registers intact" $
      execute [("b", 1)] ("a", Inc, 1, "b", Lt, 5) `shouldBe` [("a", 1), ("b", 1)]

    it "checks comparison Eq" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Eq, 3) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Eq, 6) `shouldBe` [("a", 1), ("b", 6)]

    it "checks comparison Lt" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Lt, 6) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Lt, 7) `shouldBe` [("a", 1), ("b", 6)]

    it "checks comparison Lte" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Lte, 5) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Lte, 6) `shouldBe` [("a", 1), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Lte, 7) `shouldBe` [("a", 1), ("b", 6)]

    it "checks comparison Gt" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Gt, 6) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Gt, 5) `shouldBe` [("a", 1), ("b", 6)]

    it "checks comparison Gte" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Gte, 7) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Gte, 6) `shouldBe` [("a", 1), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Gte, 5) `shouldBe` [("a", 1), ("b", 6)]

    it "checks comparison Neq" $ do
      execute [("b", 6)] ("a", Inc, 1, "b", Neq, 6) `shouldBe` [("a", 0), ("b", 6)]
      execute [("b", 6)] ("a", Inc, 1, "b", Neq, 7) `shouldBe` [("a", 1), ("b", 6)]

    it "executes the example" $
      executeProgram exampleProgram `shouldBe` ([("c", -10), ("a", 1), ("b", 0)], 10)

    it "solves the example" $
      maxRegisterValue (executeProgram exampleProgram) `shouldBe` 1

    it "solves part 1" $
      maxRegisterValue (executeProgram day8Input) `shouldBe` 3880

    describe "part 2" $ do
      it "correctly evaluates the instructions of the sample program" $ do
        execute2 ([], 0) (parseInstruction (head (lines exampleProgram))) `shouldBe` ([("b", 0)], 0)
        execute2 ([("b", 0)], 0) (parseInstruction (lines exampleProgram !! 1)) `shouldBe` ([("a", 1), ("b", 0)], 1)
        execute2 ([("a", 1), ("b", 0)], 1) (parseInstruction (lines exampleProgram !! 2)) `shouldBe` ([("c", 10),("a", 1), ("b", 0)], 10)
        execute2 ([("c", 10),("a", 1), ("b", 0)], 10) (parseInstruction (lines exampleProgram !! 3)) `shouldBe` ([("c", -10),("a", 1), ("b", 0)], 10)

      it "solves part 2" $
        snd (executeProgram day8Input) `shouldBe` 5035

exampleProgram="b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
