module Day14
 ( hexStringToBinaryString
 , numOnes
 , day14Input
 ) where

import Day10
import Numeric
import Data.Char

numOnes :: [String] -> Int
numOnes xs = length $ filter (=='1') (concat xs)

hexStringToBinaryString :: String -> [String]
hexStringToBinaryString s = [ toBinary i | i <- [0..127]]
  where toBinary i = showIntAtBase 2 intToDigit (num i) ""
        num i = fst $ head (readHex (hex i))
        hex i = knotHash (s ++ "-" ++ show i)

day14Input = "wenycdww"
