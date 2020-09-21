module Utils
( table
, cfst
, csnd
, ctrd
, avg
, cround
, takeWhileInclusive
, maybeLast
, (//)
) where

import Data.List


table :: [String] -> [[String]] -> [String]
table h xs = (tableRow h l) : (replicate (3 * l + 6) '-')
             : (tableBody xs l)
           where l = last $ sort $ map length(h ++ concat xs)

tableBody :: [[String]] -> Int -> [String]
tableBody (x:xs) l = tableRow x l : tableBody xs l
tableBody _ _ = []

tableRow :: [String] -> Int -> String
tableRow (x:xs) l = x ++ (replicate (l - (length x) + 2) ' ')
                      ++ (tableRow xs l)
tableRow _ _ = ""

cround :: Double -> Double
cround n = (fromInteger $ round $ n * (100)) / (100.0)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
                                         
maybeLast :: [a] -> Maybe a
maybeLast xs = if length xs > 0 then Just $ last xs else Nothing

cfst :: (a, a, a) -> a
cfst (x, _, _) = x

csnd :: (a, a, a) -> a
csnd (_, x, _) = x

ctrd :: (a, a, a) -> a
ctrd (_, _, x) = x

avg :: [Integer] -> Integer
avg xs | (length xs > 0)  = (sum xs) `div` (toInteger $ length xs)
       | otherwise = 0

infixl 7 //
(//) :: Integer -> Integer -> Integer
a // b = a `div` b