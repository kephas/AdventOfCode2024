module Advent where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List (sort)

day1_1 :: String -> Int
day1_1 input =
  input
    & lines
    & map (readNums >>> toPair)
    & unzip
    & sort2
    & uncurry (zipWith distance)
    & sum
 where
  sort2 (list1, list2) = (sort list1, sort list2)
  distance a b = abs $ a - b

readNums :: (Read a) => String -> [a]
readNums = words >>> map read

toPair [a, b] = (a, b)

day1_2 :: String -> Int
day1_2 input =
  list1
    & map (similarity 0 list2)
    & sum
 where
  (list1, list2) = input & lines & map (readNums >>> toPair) & unzip
  similarity :: Int -> [Int] -> Int -> Int
  similarity count [] num = count * num
  similarity count (other : rest) num = similarity (count + (if num == other then 1 else 0)) rest num

day2_1 :: String -> Int
day2_1 = day2 isSafe

day2_2 :: String -> Int
day2_2 = day2 $ allRemovals >>> any isSafe

day2 :: (Num a, Ord a, Read a) => ([a] -> Bool) -> String -> Int
day2 checkSafe input =
  input
    & lines
    & map (readNums >>> checkSafe)
    & filter id
    & length

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe (x : y : rest) = go (compare x y) (x : y : rest)
 where
  go prevDir (x : y : rest) =
    (curDir /= EQ && curDir == prevDir && abs (x - y) < 4) && (null rest || go curDir (y : rest))
   where
    curDir = compare x y

allRemovals :: [a] -> [[a]]
allRemovals list = do
  removeIndex list <$> [1 .. (length list)]

removeIndex :: [a] -> Int -> [a]
removeIndex [] _ = []
removeIndex l 0 = l
removeIndex (x : xs) 1 = xs
removeIndex (x : xs) n = x : removeIndex xs (n - 1)
