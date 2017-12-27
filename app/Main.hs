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
import Day10
import Day11
import Day12
import Day13

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
  putStrLn ("  - part 2: " ++ show day3part2)
  putStrLn "Day 4:"
  putStrLn ("  - part 1: " ++ show (countValid (map words day4Input)))
  putStrLn ("  - part 2: " ++ show (countValid2 (map words day4Input)))
--  putStrLn "Day 5:"
--  putStrLn ("  - part 1: " ++ show (stepsToExit day5Input))
--  putStrLn ("  - part 2: " ++ show (stepsToExit2 day5Input))
  putStrLn "Day 6:"
  putStrLn ("  - part 1 + 2: " ++ show (stepsUnique day6Input))
  putStrLn "Day 7:"
  putStrLn (" - part 1: " ++ bottom (Day7.parseInput day7Input))
  putStrLn (" - part 2: " ++ show (fixedWeight day7Input))
  putStrLn "Day 8:"
  putStrLn ("  - part 1: " ++ show (maxRegisterValue (executeProgram day8Input)))
  putStrLn ("  - part 2: " ++ show (snd (executeProgram day8Input)))
  putStrLn "Day 9:"
  putStrLn ("  - part 1: " ++ show (score day9Input))
  putStrLn ("  - part 2: " ++ show (nonCancelledGarbage day9Input))
  putStrLn "Day 10:"
  putStrLn ("  - part 1: " ++ show (hash1 256 day10Input))
  putStrLn ("  - part 2: " ++ knotHash day10Input2)
  putStrLn "Day 11:"
  putStrLn ("  - part 1: " ++ show (distance (parseDirections day11Input)))
  putStrLn ("  - part 2: " ++ show (maxDistance (parseDirections day11Input)))
  putStrLn "Day 12:"
  putStrLn ("  - part 1: " ++ show (length (reachable 0 (parseNodes day12Input) [])))
  putStrLn ("  - part 2: " ++ show (length (groups (map fst parsedDay12Input) parsedDay12Input)))
  putStrLn "Day 13:"
  putStrLn ("  - part 1: " ++ show (severity day13Firewall (passage day13Firewall)))
