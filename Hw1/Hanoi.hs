{-# OPTIONS_GHC -Wall #-}

module Hanoi
    ( hanoi
    ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n >= 1    = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
  | otherwise = []
