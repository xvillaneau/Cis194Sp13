{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jlA jlB = Append m jlA jlB
  where m = mappend (tag jlA) (tag jlB)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Exercise 2

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ Empty = 0
sizeJ (Single _ _) = 1
sizeJ (Append s _ _) = getSize $ size s

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append s jlA jlB)
  | i < 0 = Nothing
  | i >= getSize (size s) = Nothing
  | i < sizeA = indexJ i jlA
  | otherwise = indexJ (i - sizeA) jlB
  where sizeA = sizeJ jlA

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 (Single _ _) = Empty
dropJ _ jl@(Single _ _) = jl
dropJ i jl@(Append s jlA jlB)
  | i < 0 = jl
  | i >= getSize (size s) = jl
  | i < sizeA = jl
  | otherwise = jl
  where sizeA = sizeJ jlA
