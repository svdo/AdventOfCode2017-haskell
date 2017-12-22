module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

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
  putStrLn "Day 3:"
  putStrLn ("  - part 1: " ++ show (manhattanDistance day3Input))
  putStrLn "Day 4:"
  putStrLn ("  - part 1: " ++ show (countValid (map words day4Input)))
--  putStrLn "Day 5:"
--  putStrLn ("  - part 1: " ++ show (stepsToExit day5Input))
--  putStrLn ("  - part 2: " ++ show (stepsToExit2 day5Input))
  putStrLn "Day 6:"
  putStrLn ("  - part 1 + 2: " ++ show (stepsUnique day6Input))
  putStrLn "Day 7:"
  putStrLn (" - part 1: " ++ bottom (Day7.parseInput day7Input))
  putStrLn "Day 8:"
  putStrLn ("  - part 1: " ++ show (maxRegisterValue (executeProgram day8Input)))
  putStrLn ("  - part 2: " ++ show (snd (executeProgram day8Input)))
  putStrLn "Day 9:"
  putStrLn ("  - part 1: " ++ show (score day9Input))
  putStrLn ("  - part 2: " ++ show (nonCancelledGarbage day9Input))
