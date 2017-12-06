module Main where

import Day1
import Day2

main :: IO ()
main = do
  putStrLn "Day 1:"
  putStrLn ("  - part 1: " ++ show (solveCaptcha captcha))
  putStrLn ("  - part 2: " ++ show (solveCaptcha2 captcha))
  putStrLn "Day 2:"
  putStrLn "  - part 1: "
  day2input <- readFile "app/day2.spreadsheet"
  print . checksum $ day2input
  putStrLn "  - part 2: "
  print . sumOfEvenlyDivisible $ day2input
