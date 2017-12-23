module Day10
  ( step
  , allSteps
  , performReverse
  , hash
  , day10Input
  ) where

import Data.List

type List = [Int]
type CurrentIndex = Int
type Lengths = [Int]
type SkipSize = Int
type State = (List, CurrentIndex, SkipSize, Lengths)
type ListLength = Int

hash :: ListLength -> Lengths -> Int
hash l lengths = head list * (list !! 1)
  where (list, index, skip, _) = allSteps ([0..(l-1)], 0, 0, lengths)

allSteps :: State -> State
allSteps (list, index, skip, []) = (list, index, skip, [])
allSteps state = allSteps (step state)

step :: State -> State
step (list, index, skip, lengths) = (newList, newIndex, skip+1, tail lengths)
  where newList = reverseFrom index currentLength list
        newIndex = (index + skip + currentLength) `mod` length list
        currentLength = head lengths

reverseFrom :: Int -> Int -> List -> List
reverseFrom i len xs = performReverse i xs reversedPart
  where reversedPart = reverse $ take len (drop i (cycle xs))

performReverse :: Int -> List -> List -> List
performReverse i xs [] = xs
performReverse i xs reversedPart = performReverse nextI newXs (tail reversedPart)
  where nextI = (i + 1) `mod` length xs
        newXs = take i xs ++ [head reversedPart] ++ drop (i+1) xs

day10Input = [230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167] :: [Int]
