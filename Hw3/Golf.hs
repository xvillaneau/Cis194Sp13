{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (elemIndices)

-- Exercise 1: Skips

skips :: [a] -> [[a]]
skips l = map (($ l) . nThs) [1..length l]

nThs :: Int -> [a] -> [a]
nThs n = map snd . filter fst . zip (cycle (tf n))

tf :: Int -> [Bool]
tf n = replicate (n - 1) False ++ [True]

-- Exercise 2: Local Maxima

localMaxima :: [Integer] -> [Integer]
localMaxima = map snd3 . filter locMax . triples

locMax :: Ord a => (a, a, a) -> Bool
locMax (a, b, c) = a < b && c < b

snd3 :: (a, a, a) -> a
snd3 (_, b, _) = b

triples :: [a] -> [(a, a, a)]
triples l = zip3 l (drop 1 l) (drop 2 l)

-- Exercise 3: Histogram

histogram :: [Integer] -> String
histogram l = unlines $ map (mkLine c) [i, i-1..1] ++ ["==========", "0123456789"]
  where c = counts l; i = maximum c

counts :: [Integer] -> [Int]
counts l = map (count l) [0..9]

count :: Eq a => [a] -> a -> Int
count l x = length $ elemIndices x l

mkLine :: [Int] -> Int -> String
mkLine l n = map (\i -> if i >= n then '*' else ' ') l
