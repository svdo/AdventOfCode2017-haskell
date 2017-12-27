module Day13
  ( firewall
  , caughtAt
  , updateScanners
  , passage
  , severity
  , day13Input
  , day13Firewall
  ) where

import Data.List
import Data.Maybe
import Debug.Trace

type Depth = Int
type Range = Int
type Firewall = [Range]
type Scanners = [Depth]

severity :: Firewall -> [Depth] -> Int
severity firewall caught = sum [ i * firewall !! i | i <- caught ]

passage :: Firewall -> [Depth]
passage firewall = passage' 0 firewall initialScanners
  where initialScanners = replicate (length firewall) 0

passage' :: Depth -> Firewall -> Scanners -> [Depth]
passage' depth firewall scanners
--  | trace ("passage' " ++ show depth ++ " " ++ show scanners) False  = undefined
  | depth == length scanners         = []
  | caughtAt depth firewall scanners = depth:remaining
  | otherwise                        = remaining
  where remaining = passage' (depth+1) firewall (updateScanners firewall scanners)

caughtAt  :: Depth -> Firewall -> Scanners -> Bool
caughtAt depth firewall scanners = firewall !! depth > 0 && scanners !! depth == 0

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
