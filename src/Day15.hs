module Day15
  ( generate
  , generateNext
  , generate2
  , generateNext2a
  , generateNext2b
  , day15Input
  ) where

import Data.List
import Data.Bits

generate :: ((Int,Int),Int) -> Int -> ((Int,Int), Int)
generate ((a,b),matches) 0 = ((a,b),matches)
generate ((a,b),matches) count = generate ((nextA,nextB),newMatches) (count-1)
  where (nextA,nextB) = generateNext (a,b)
        newMatches
          | (.&.) nextA 65535 == (.&.) nextB 65535 = matches + 1
          | otherwise                              = matches

generateNext :: (Int,Int) -> (Int,Int)
generateNext (a,b) = (a*factorA `rem` divisor, b*factorB `rem` divisor)


generate2 :: ((Int,Int),Int) -> Int -> ((Int,Int), Int)
generate2 ((a,b),matches) 0 = ((a,b),matches)
generate2 ((a,b),matches) count = generate2 ((nextA,nextB),newMatches) (count-1)
  where (nextA,nextB) = (generateNext2a a, generateNext2b b)
        newMatches
          | (.&.) nextA 65535 == (.&.) nextB 65535 = matches + 1
          | otherwise                              = matches

generateNext2a :: Int -> Int
generateNext2a a
  | (.&.) next 3 == 0 = next
  | otherwise         = generateNext2a next
  where next = a*factorA `rem` divisor

generateNext2b :: Int -> Int
generateNext2b b
  | (.&.) next 7 == 0 = next
  | otherwise         = generateNext2b next
  where next = b*factorB `rem` divisor

factorA = 16807 :: Int
factorB = 48271 :: Int
divisor = 2147483647 :: Int
day15Input = (116, 299) :: (Int,Int)
