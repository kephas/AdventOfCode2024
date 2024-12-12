module Advent where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List (sort)

day1_1 :: String -> Int
day1_1 input =
  input
    & lines
    & map (words >>> map read >>> toPair)
    & unzip
    & sort2
    & uncurry (zipWith distance)
    & sum
 where
  sort2 (list1, list2) = (sort list1, sort list2)
  distance a b = abs $ a - b

toPair [a, b] = (a, b)

day1_2 :: String -> Int
day1_2 input =
  list1
    & map (similarity 0 list2)
    & sum
 where
  (list1, list2) = input & lines & map (toPair . map read . words) & unzip
  similarity :: Int -> [Int] -> Int -> Int
  similarity count [] num = count * num
  similarity count (other : rest) num = similarity (count + (if num == other then 1 else 0)) rest num
