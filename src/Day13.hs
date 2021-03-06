module Day13
  ( firewall
  , caughtAt
  , updateScanners
  , updateScanners'
  , passage
  , passageWithoutGettingCaught
  , tryPassage
  , severity
  , day13Input
  , day13Firewall
  , day13b
  ) where

import Data.List
import Data.Maybe
import Debug.Trace

type Depth = Int
type Range = Int
type Firewall = [Range]
type Scanners = [Depth]
type Delay = Int

severity :: Firewall -> [Depth] -> Int
severity firewall caught = sum [ i * firewall !! i | i <- caught ]

passageWithoutGettingCaught :: Firewall -> Delay -> Delay
passageWithoutGettingCaught firewall delay
  | delay `mod` 100000 == 0 && trace ("try passage without getting caught: " ++ show delay) False = undefined
  | delay > 4000000                = -1
  | tryPassage 0 firewall scanners = delay
  | otherwise                      = passageWithoutGettingCaught firewall (delay+1)
  where scanners = updateScanners' firewall (replicate (length firewall) 0) delay

passage :: Firewall -> [Depth]
passage firewall = passage' 0 firewall initialScanners
  where initialScanners = replicate (length firewall) 0

tryPassage :: Depth -> Firewall -> Scanners -> Bool
tryPassage depth firewall scanners
--  | trace ("try passage: depth = " ++ show depth) False = undefined
  | depth == length scanners         = True
  | caughtAt depth firewall scanners = False
  | otherwise                        = tryPassage (depth+1) firewall (updateScanners firewall scanners)

passage' :: Depth -> Firewall -> Scanners -> [Depth]
passage' depth firewall scanners
--  | trace ("passage' " ++ show depth ++ " " ++ show scanners) False  = undefined
  | depth == length scanners         = []
  | caughtAt depth firewall scanners = depth:remaining
  | otherwise                        = remaining
  where remaining = passage' (depth+1) firewall (updateScanners firewall scanners)

caughtAt  :: Depth -> Firewall -> Scanners -> Bool
caughtAt depth firewall scanners = firewall !! depth > 0 && scanners !! depth == 0

updateScanners' :: Firewall -> Scanners -> Int -> Scanners
updateScanners' firewall scanners count = [ update index | index <- [0..length scanners-1] ]
  where update i
          | firewall !! i == 0    = 0
          | next < 0              = abs next
          | next >= firewall !! i = -(firewall !! i - (next - firewall !! i) - 2)
          | otherwise             = next
             where next = nextValue i
        nextValue i
          | scanners !! i < 0 = abs (scanners !! i) - numToAdd
          | otherwise         = scanners !! i + numToAdd
             where numToAdd = count `mod` (2 * (firewall!!i-1))

updateScanners :: Firewall -> Scanners -> Scanners
updateScanners firewall scanners = [ update index | index <- [0..length scanners-1] ]
  where update i
          | firewall !! i > 0 = nextScannerPos (scanners!!i) (firewall!!i - 1)
          | otherwise         = 0
        nextScannerPos prev max
          | prev >= 0 && prev+1 <= max       = prev+1
          | prev >= 0                        = -(max - 1)
          | prev < 0 && (abs prev)-1 >= 0    = -((abs prev)-1)

firewall :: [(Depth,Range)] -> [Range]
firewall input = [range i | i <- [0 .. maxDepth]]
  where
    maxDepth = maximum depths
    range i = fromMaybe 0 (lookup i input)
    (depths, ranges) = unzip input

day13Firewall = firewall day13Input
day13Input = [(0, 3), (1, 2), (2, 4), (4, 4), (6, 5), (8, 6), (10, 8), (12, 8), (14, 6), (16, 6), (18, 8), (20, 8), (22, 6), (24, 12), (26, 9), (28, 12), (30, 8), (32, 14), (34, 12), (36, 8), (38, 14), (40, 12), (42, 12), (44, 12), (46, 14), (48, 12), (50, 14), (52, 12), (54, 10), (56, 14), (58, 12), (60, 14), (62, 14), (66, 10), (68, 14), (74, 14), (76, 12), (78, 14), (80, 20), (86, 18), (92, 14), (94, 20), (96, 18), (98, 17)] :: [(Depth,Range)]

day13b :: [(Depth,Range)] -> Int
day13b input = head [i | i <- [0..], not $ caught i] where
    caught i = or [(d + i) `mod` (2 * n - 2) == 0 | (d, n) <- input]
