module Day6 (
  stepsUnique
  , redistribute
  , update
  , distribute
  , setZero
  , day6Input
) where

import Data.List
import Data.Maybe

stepsUnique xs = redistribute xs [] 0

redistribute :: [Int] -> [[Int]] -> Int -> Int
redistribute xs seen count
  | xs `elem` seen = count
  | otherwise      = redistribute (update xs) (xs : seen) (count + 1)

update :: [Int] -> [Int]
update xs = distribute max (setZero maxElemIndex xs) startIndex
  where maxElemIndex = fromJust $ elemIndex max xs
        max = maximum xs
        startIndex
          | (maxElemIndex + 1) < length xs = maxElemIndex + 1
          | otherwise                      = 0

distribute :: Int -> [Int] -> Int -> [Int]
distribute 0 xs from = xs
distribute n xs from = distribute (n-1) (prefix ++ [cur+1] ++ suffix) nextFrom
  where prefix = take from cxs
        cur = head $ drop from cxs
        suffix = take (length xs - length prefix - 1) (drop (from+1) cxs)
        cxs = cycle xs
        nextFrom
          | (from + 1) < length xs = from + 1
          | otherwise              = 0

setZero :: Int -> [Int] -> [Int]
setZero index xs = take (length left - 1) left ++ [0] ++ right
  where (left, right) = splitAt (index + 1) xs

day6Input = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11] :: [Int]
