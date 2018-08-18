{-# OPTIONS_GHC -Wall #-}

module HighOrder where

import Data.List ((\\))

-- Wholemeal programming

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n = n + fun2' (n `div` 2)
  | otherwise = fun2' (3 * n + 1)

evenOdd :: Integral n => n -> n -> n -> n
evenOdd a b x = if even x then a else b

fun2Step :: Integral n => (n, n) -> (n, n)
fun2Step (_, 1) = (0, 0)
fun2Step (_, n) = (evenOdd n 0 n, evenOdd (n `div` 2) (n*3+1) n)

fun1 :: [Integer] -> Integer
fun1 = foldr (\x -> if even x then ((x-2)*) else id) 1

fun2 :: Integer -> Integer
fun2 n = sum $ map fst (takeWhile ((>0).snd) $ iterate fun2Step (0, n))

-- Folding with trees

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node n Leaf _ Leaf) = n
height (Node _ l _ r) = max (height l) (height r)

lowest :: Tree a -> Tree a -> Tree a
lowest a b = if height a <= height b then a else b

insertBalanced :: Integer -> a -> Tree a -> Tree a
insertBalanced n x Leaf = Node n Leaf x Leaf
insertBalanced _ x (Node n l v r) =
  if height l <= height r
    then Node n (next l) v r
    else Node n l v (next r)
  where next = insertBalanced (n+1) x

foldTree :: [a] -> Tree a
foldTree = foldr (insertBalanced 0) Leaf

-- More folds!

xor :: [Bool] -> Bool
xor = foldr (/=) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- Finding primes

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map res $ ns \\ rms
  where ns = [1..n]; nrs = [1..(div n 3)]
        ijs = cartProd nrs nrs
        rm (i, j) = i + j + 2*i*j
        res i = 2 * i + 1
        rms = filter (<= n) (map rm ijs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
