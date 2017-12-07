module Day3
  ( elementsInRing
  , ringOf
  , coordinateOf
  , moveInRing
  , day3Input
  , manhattanDistance
  ) where

elementsInRing :: Int -> Int
elementsInRing r = sizeOfRing r ^ 2

sizeOfRing :: Int -> Int
sizeOfRing r = 2 * r + 1

ringOf :: Int -> Int
ringOf 1 = 0
ringOf i
  | i-1 == elementsInRing ringOfPrevious = 1 + ringOfPrevious
  | otherwise                            = ringOfPrevious
      where ringOfPrevious = ringOf (i-1)

coordinateOf :: Int -> (Int,Int)
coordinateOf 1 = (0,0)
coordinateOf i = moveInRing ring distanceInring
  where ring = ringOf i
        elementsInPreviousRing = elementsInRing (ring-1)
        distanceInring = i - elementsInPreviousRing - 1

moveInRing :: Int -> Int -> (Int,Int)
moveInRing r 0 = (r, r-1)
moveInRing r n = (prevX + dx, prevY + dy)
  where (prevX, prevY) = moveInRing r (n-1)
        dx
         | n > 3*toCorner-1 = 1
         | n > 2*toCorner-1 = 0
         | n > toCorner-1   = -1
         | otherwise        = 0
        dy
         | n > 3*toCorner-1 = 0
         | n > 2*toCorner-1 = 1
         | n > toCorner-1   = 0
         | otherwise        = -1
        toCorner = 2*r

manhattanDistance :: Int -> Int
manhattanDistance i = (abs x) + (abs y)
  where (x,y) = coordinateOf i

day3Input = (265149 :: Int)
