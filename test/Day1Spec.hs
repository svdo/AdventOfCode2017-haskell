module Day1Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Day1

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "day 1" $ do
    it "converts string to list of ints" $
      parseInput "123" `shouldBe` ([1, 2, 3] :: [Int])

    describe "part 1" $ do
      it "solves captcha 1234" $
        solveCaptcha "1234" `shouldBe` 0

      it "solves captcha 1122" $
        solveCaptcha "1122" `shouldBe` 3

      it "solves captcha 1111" $
        solveCaptcha "1111" `shouldBe` 4

      it "solves captcha 91212129" $
        solveCaptcha "91212129" `shouldBe` 9

      it "gives the right answer" $
        solveCaptcha captcha `shouldBe` 1049

    describe "part 2" $ do
      it "can circularly shift the input" $
        shift 3 [1,2,3,4,5,6,7,8] `shouldBe` [4,5,6,7,8,1,2,3]

      it "is unchanged when shifted with length" $ property $ forAll orderedList $
        \s -> shift (length s) s == s

      it "solves captcha 1212" $
        solveCaptcha2 "1212" `shouldBe` 6

      it "solves captch 1221" $
        solveCaptcha2 "1221" `shouldBe` 0

      it "solves captcha 123425" $
        solveCaptcha2 "123425" `shouldBe` 4

      it "solves captcha 123123" $
        solveCaptcha2 "123123" `shouldBe` 12

      it "solves captcha 12131415" $
        solveCaptcha2 "12131415" `shouldBe` 4

      it "gives the right answer" $
        solveCaptcha2 captcha `shouldBe` 1508
