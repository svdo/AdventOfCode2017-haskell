module Day14
 ( hexStringToBinaryString
 , numOnes
 , binaryStringsToNodeList
 , findGroups
 , toInts
 , replace
 , countRegions
 , markAllRegions
 , markRegion
 , findFirst
 , day14Input
 ) where

import Day10
import Day12
import Numeric
import Data.Char
import Debug.Trace
import Data.List
import Data.Maybe

numOnes :: [String] -> Int
numOnes xs = length $ filter (=='1') (concat xs)

hexStringToBinaryString :: String -> [String]
hexStringToBinaryString s = [ toBinary i | i <- [0..127]]
  where toBinary i = fullLength (showIntAtBase 2 intToDigit (num i) "")
        num i = fst $ head (readHex (hex i))
        hex = fastKnotHash s

fullLength :: String -> String
fullLength xs = drop (length xs) (replicate 128 '0') ++ xs

fastKnotHash :: String -> Int -> String
fastKnotHash s i
  | s == day14Input = precomputedKnotHash !! i
  | otherwise       = knotHash (s ++ "-" ++ show i)

binaryStringsToNodeList :: [String] -> [Node]
binaryStringsToNodeList xs = filter ((/= -1) . fst) [toNode row col xs | row <- [0..max], col <- [0..max]]
  where max = length xs - 1

toNode :: Int -> Int -> [String] -> Node
toNode row col xs
--  | trace ("toNode " ++ show row ++ "," ++ show col) False = undefined
  | xs !! row !! col == '1' = (row*length xs + col, reachableFrom row col xs)
  | otherwise               = (-1, [])

reachableFrom :: Int -> Int -> [String] -> [Int]
reachableFrom row col xs
--  | trace ("reachableFrom " ++ show row ++ "," ++ show col) False = undefined
--  | otherwise
    = filter (/= -1) [up,down,left,right]
  where left
          | col > 0 && xs !! row !! (col-1) == '1'               = row*size + col - 1
          | otherwise                                            = -1
        right
          | col < (length xs - 1) && xs !! row !! (col+1) == '1' = row*size + col + 1
          | otherwise                                            = -1
        up
          | row > 0  && xs !! (row-1) !! col == '1'              = (row-1)*size + col
          | otherwise                                            = -1
        down
          | row < (length xs - 1) && xs !! (row+1) !! col == '1' = (row+1)*size + col
          | otherwise                                            = -1
        size = length xs

toInts :: [String] -> [[Int]]
toInts xs = [map readInt s| s <- xs]
  where readInt c = read [c] :: Int

findGroups :: [[Int]] -> [Int]
findGroups rows = []

replace :: (Int,Int) -> Int -> [[Int]] -> [[Int]]
replace (row,col) new grid = take row grid ++ (newRow:drop (row+1) grid)
  where newRow = take col oldRow ++ (new:drop (col+1) oldRow)
        oldRow = grid !! row

countRegions :: [[Int]] -> Int
countRegions grid = maximum (concat $ markAllRegions grid) - 1

markAllRegions :: [[Int]] -> [[Int]]
markAllRegions grid = markAllRegions' grid 2

markAllRegions' :: [[Int]] -> Int -> [[Int]]
markAllRegions' grid newValue
  | 1 `elem` (concat markOneRegion) = markAllRegions' markOneRegion (newValue+1)
  | otherwise                       = markOneRegion
  where markOneRegion = markRegion grid newValue

markRegion :: [[Int]] -> Int -> [[Int]]
markRegion grid new = result
  where start = findFirst 1 grid
        result = case start of
                   Nothing -> grid
                   Just coord -> markRegionFrom coord grid new

markRegionFrom :: (Int,Int) -> [[Int]] -> Int -> [[Int]]
markRegionFrom (row,col) grid new
--  | trace (concat ["markRegionFrom(",show row,",",show col,")"]) False = undefined
--  | otherwise
  = up
  where replaceCurrent = replace (row,col) new grid
        up
          | row > 0 && grid !! (row-1) !! col == 1 = markRegionFrom (row-1,col) down new
          | otherwise = down
        down
          | row < length grid - 1 && grid !! (row+1) !! col == 1 = markRegionFrom (row+1,col) right new
          | otherwise = right
        right
          | col < length grid - 1 && grid !! row !! (col+1) == 1 = markRegionFrom (row,col+1) left new
          | otherwise = left
        left
          | col > 0 && grid !! row !! (col-1) == 1 = markRegionFrom (row,col-1) replaceCurrent new
          | otherwise = replaceCurrent

findFirst :: Int -> [[Int]] -> Maybe (Int,Int)
findFirst num grid = result
  where maybeRow = findIndex (elem num) grid
        result = case maybeRow of
                   Nothing  -> Nothing
                   Just row -> Just (row, fromJust $ elemIndex num (grid!!row))

precomputedKnotHash :: [String]
precomputedKnotHash = ["cad143feddda59e4857583819cf69511","10552123137842600e8f476e09a5e0ea","ee1a8f8b5aa38a67fde57d5ac18f0693","d4a80f34ccf1d2be17353eed3d8499c7","923605e5c3f12bfe66b078416f4acea7","c0f1939e3970043305b27f998a24a71a","bbb73a0ec737cdc4d034e92930a29fe8","ce97e93072351ee8ba6f223394230a48","43cddb92ced9e44a83f3a34fe2940e9a","5e3cd2772d42c090c86ab07cdd2fa2c6","c6aad24cfc5d0541dfa2be3ba58ba766","5fbb7a96c1decef5d3939fd2fe7a8e2b","f78b15c9454d9a9119493f9e3b15c4b8","0bd5146b94fa4109d737f67428f3211f","9d75efc59fdfe99428b0feb186f3abf6","703905a9b435e930f08ca1480b04dafd","8390e4de3eb402e8633100d3557efe1d","8348e06068dfa10c6ea9e9d3a18bc442","8ff374cdde57e2f2bf4eb2010ba85fe2","7a7f5663fe7064aa1d7cc318f34fdbad","e4f104de04c6953eb3bb61541addca96","63b0a6c24d4e6a293dcd37a9b6820da0","dc11e80bcfaa95f828804f0df238e9ef","90bc211f37bbe79ca10d1d46161eebf1","d8ac100f047828555a3076ab2b6d940f","1f0805d697a6abdad29fefa9382326b2","a6973a589e4bbadf3c65fd2fb5bf7614","9f681c25320eee4f347e9d2c1f4918e6","9cb310b00d0bb33a61383a03907b159e","11f15f27757e5128e09d992ea9882223","7a5224e74a3bf4624c564c708a1555e0","ee66981821cfea98e9aad49e02f8f196","638e05cb67c319ba455d3609a51748f9","2b55f1212f8cae70cd6edd45add54cdc","10e1b8214aa60cdfdebf8056ba5c8b8d","125d473e21f0491556e4f4d65c0fd6ae","3cfeebab06f9c8a90592432d42c38feb","3d0e1e4ea86ef0faa76b9c842c630135","52e74374aad0b3aff2e5b1312beab406","5064e1e9ee4200b6318e8762fb069f1e","86049727ad89ee16131e0bd3679013df","1feb2d46570ef3626cf5199675456415","d62901497cccc583f476b8e8341150e6","2ff28bc44773dde741458c998db7dd6a","432984689ba9d051c1fc8712bc29bcb4","a1d7513fc22ec9ca892b680b30687e10","c02c55313a2ee870d01dedf7404fc519","22dac143dfafff7f579e2d3b386e3330","48a309d4008d124dd17648983b91ab92","3a6c20a4f03f3e758874f2817ac396f6","f7e53ab217b6da16da0b523373fce028","e94e34d9bbe06613ca9823a3bfd4469b","d0d7965b42005157f74f7dd49c396f55","21c65cc6e8023d04e000ee29060627ae","531888ed4b01625739bc2936d8b415b2","72285b8cf861d60db72e921c25e43128","852a4be5ebd674a0df06b3581a4b068d","278e48d095b6e598627e4b4b5f5f82f1","1ef52340bbd34f30a7ab4c2958449278","cf6466c5a077fca09449ed9f3fa814af","e0de233971deeb96baa6c1f377d1a2dc","07b2f7bdcdfa03d309b7802c172f6654","07594887cb752d7f48af09e3c652789c","b4cfa0c16e3771649982e85905535df7","940335fb09588dd149dea7cd011d5fea","5329ba085b1f8a315a2c7d270eec09f0","ae6dcccfe3fbb57e5f4f816592367d3e","9bbc6a011104e1ba785cd0c6b5101184","19dded1889d84de6e67d32ac88f92d92","759a41980f9d99b8a722d29e53577d35","ae2e5b863a82a84a6038bab8b07c54c5","26d2785949dfa4e3404a58bbab331a6f","dafc55df96cc93ed54f42ab5794fe564","64e8f3f8e81ca081ac921cba23ea898a","36b450f2bb86d39ba66fa948bc3c708d","6afa5f608dd0becb8c7585ef4b2382fe","c333da3109dcdedf6dc844e5a2d1a61e","e685f90feba1215b6dce4d3fff6ad435","a3333a9e81c6e00c60a389766e0938fc","6addafaf50df61abe8e97424b89442cd","e0bc6e95a85c4244cea3b489d19f80cb","b3f96481bec2d79b577c810a6f316504","05f5e3e8eb3f818e020e28a779fe290d","6963c2188d11bcebd24b2680af817872","630f5e0cfb4daac314bdff5a96958866","5d467ea18552d774a35e8e3bcdec16cf","ab8a76ca95034f242a87edb3e67ff9f3","bfb5efbd7a7d3d09d344d26750f58e62","0837c98df6870c86fa47b3d3f6ffa2f6","307d2fa1c6cee62641925521f5d19e16","1433a8da9307a707c951bb694661d8d4","44962e287407880320955ad9f3a50d41","0af5055f821634d682463a673efed05a","2806cb9031bd80a72b37f1c323704fec","0d0aab212d0646bfad9c8449f98c2afc","a1c253f4f6c1ab077cbf6cc64c2a0959","ef852a962ffc61c5392c1016d35d2c10","ec4f9d87f1ba5c0e652e0569496502a9","d66fb46e6c172a20fbc58dbf6d191278","575f941fe7efe38455e0730c6eb2bd47","c733f2e8b9b0acbaf3aa1a4b6258c201","4717e6efabee7c0c9396816b625c04b9","073716ea47073083b40cb061f3a6f0f3","d007f024c4e04ceb4ff647e0671b3ddf","71fe56b70945ee243a3f397fb7ac8333","34a1b8ab306bb9f815c1cae5ea4db575","b473ab4d4372f1bf7853b2499c84b127","64aad5017cde88a3a3083fc7b2fedf53","df9668de590952c86d9078ccde1e60dc","0fe2f4aa7852cda73ac7914564dba8cd","ddd2948ca48466c4f8b3e897cd6febe8","8371d9a6327f905a85e587be8fe67248","2a18b8bf8c903cc79dc59d57159c9a53","c9635a241903c485a0c2c8c899654657","b336a9a24eeb9ede4d6cfeaab7349f02","4628664aaa9a18e792905b8710ece44b","d07ad6ce0c5b0ec3c1ed47e98edd03fa","bbe521f3435269744c3675ef1befcd59","f413a4e91a740f131b13a139ccf5d8a9","14692c47d78cb4732aa258a232a23b53","4d54cd87de29eb9cca2223130fa3d770","c55b5a7246d2eee433d66ff3d02ede71","c4be54b2a44977ebe7a83b0f648c1f61","99580ddf21ea26bc4958db9568697965","22d63e068d8b5ad685cdb43d755d7fd0","ac07e1c08f09b4ef93e77336efda88db","4b9af032204a676efbac13aac641dec7","146502bd9e15a1587ddc2102c671c841"]

day14Input = "wenycdww"
