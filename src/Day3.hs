module Day3
  ( elementsInRing
  , ringOf
  , coordinateOf
  , moveInRing
  , day3Input
  , manhattanDistance
  , grid
  , day3part2
  ) where

import Data.List
import Data.Maybe

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
manhattanDistance i = abs x + abs y
  where
    (x, y) = coordinateOf i

-- part 2

type Coordinate = (Int,Int)
type Grid = [(Coordinate, Int)]

grid :: Int -> Grid
grid i = createGrid i []

createGrid :: Int -> Grid -> Grid
createGrid 1 [] = [((0,0), 1)]
createGrid i grid = new:smallerGrid
  where new = ((x,y), newValue)
        (x,y) = coordinateOf i
        newValue = west + south + southWest + east + southEast + northWest + north + northEast
        west = lookupValue smallerGrid (x-1, y)
        south = lookupValue smallerGrid (x, y+1)
        southWest = lookupValue smallerGrid (x-1, y+1)
        east = lookupValue smallerGrid (x+1, y)
        southEast = lookupValue smallerGrid (x+1, y+1)
        northWest = lookupValue smallerGrid (x-1, y-1)
        north = lookupValue smallerGrid (x, y-1)
        northEast = lookupValue smallerGrid (x+1, y-1)
        smallerGrid = createGrid (i-1) grid

lookupValue :: Grid -> Coordinate -> Int
lookupValue grid coordinate = fromMaybe 0 (lookup coordinate grid)

day3part2 :: Int
day3part2 = largestValue 1

largestValue :: Int -> Int
largestValue i
  | value > day3Input = value
  | otherwise         = largestValue (i+1)
  where value = snd(head(grid i))


day3Input = 265149 :: Int
