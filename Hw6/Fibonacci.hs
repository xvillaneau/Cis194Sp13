{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib n | n <= 1    = 1
      | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fib' :: (Integer, Integer) -> (Integer, Integer)
fib' (a, b) = (b, a + b)

fibs2 :: [Integer]
fibs2 = map (uncurry (+)) $ iterate fib' (1, 0)

-- Exercise 3

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream i s) = i : streamToList s

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat i = Stream i (streamRepeat i)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream i s) = Stream (f i) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f i = Stream i (streamFromSeed f (f i))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a sa) (Stream b sb)
  = Stream a $ Stream b $ interleaveStreams sa sb

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap evPow2 nats1
  where nats1 = streamFromSeed (+1) 1 :: Stream Integer
        pow2 = floor . (logBase 2 :: Double -> Double) . fromIntegral
        evPow2 n = maximum $ filter ((== 0) . mod n . (2^)) [0..pow2 n]

ruler2 :: Stream Integer
ruler2 = ruler' 0 where
  ruler' n = interleaveStreams (streamRepeat n) (ruler' (n+1))

-- Exercise 6

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate = streamMap negate
  (+) (Stream a sa) (Stream b sb) = Stream (a + b) (sa + sb)
  (*) (Stream a0 a') b@(Stream b0 b')
    = Stream (a0*b0) (streamMap (*a0) b' + a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Stream a0 a') b@(Stream b0 b')
    = Stream (a0 `div` b0) (streamMap (`div` b0) (a' - (a/b) * b'))

fibs3 :: Stream Integer
fibs3 = f where (Stream _ f) = x / (1 - x - x ^ (2 :: Integer))

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11)
    = Matrix (a00*b00+a01*b10) (a00*b01+a01*b11)
             (a10*b00+a11*b10) (a10*b01+a11*b11)

fm :: Matrix
fm = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 1
fib4 n = f0 where (Matrix f0 _ _ _) = fm ^ n

fibs4 :: [Integer]
fibs4 = map fib4 [0..]
