module Day10
  ( step
  , oneRound
  , performReverse
  , hash1
  , knotHash
  , sparseHash
  , denseHash
  , cut
  , day10Input
  , day10Input2
  , processedInput
  ) where

import Data.List
import Data.Char (ord)
import Data.Bits
import Numeric (showHex)

type List = [Int]
type CurrentIndex = Int
type Lengths = [Int]
type SkipSize = Int
type State = (List, CurrentIndex, SkipSize, Lengths)
type ListLength = Int
type NumRounds = Int

hash1 :: ListLength -> Lengths -> Int
hash1 l lengths = head list * (list !! 1)
  where (list, index, skip, _) = oneRound ([0..(l-1)], 0, 0, lengths)

cut :: Int -> [Int] -> [[Int]]
cut size xs = [ take size $ drop (i*size) xs | i <- [0..((length xs `div` size)-1)]]

knotHash :: String -> String
knotHash input = toHex $ denseHash $ sparseHash 256 (processedInput input) 64

processedInput input = map ord input ++ [17, 31, 73, 47, 23]

toHex :: [Int] -> String
toHex xs = concat [asHex i | i <- xs]
  where asHex i
          | length (hex i) == 2 = hex i
          | otherwise           = "0" ++ hex i
        hex i = showHex i ""

denseHash :: [Int] -> [Int]
denseHash sparse = map (foldl xor 0) (cut 16 sparse)

sparseHash :: ListLength -> Lengths -> NumRounds -> [Int]
sparseHash l lengths rounds = list
  where (list, _, _, _) = manyRounds rounds lengths ([0..(l-1)], 0, 0, lengths)

manyRounds :: NumRounds -> Lengths -> State -> State
manyRounds 1 _ state = state
manyRounds numRounds lengths state = manyRounds (numRounds - 1) lengths nextState
  where nextState = oneRound (list, index, skip, lengths)
        (list, index, skip, _) = (oneRound state)

oneRound :: State -> State
oneRound (list, index, skip, []) = (list, index, skip, [])
oneRound state = oneRound (step state)

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
day10Input2 = "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"
