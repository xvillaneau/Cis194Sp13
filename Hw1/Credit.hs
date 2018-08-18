{-# OPTIONS_GHC -Wall #-}

module Credit
    ( validate
    ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = r : toDigitsRev q
    where (q, r) = divMod n 10

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' False . reverse

doubleEveryOther' :: Bool -> [Integer] -> [Integer]
doubleEveryOther' _ []   = []
doubleEveryOther' t (x:xs) = next : doubleEveryOther' (not t) xs
  where next = if t then 2 * x else x

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
