module Main where

import Data.List (sort, transpose)

main :: IO ()
main = do
  content <- readFile "input/1.txt"
  print $ distance content
  print $ similarity content

-- Day 1 --

toInt :: String -> Integer
toInt = read

intMatrix :: String -> [[Integer]]
intMatrix = map (map toInt . words) . lines

distance :: String -> Integer
distance =
  sum
    . map (abs . (\x -> head x - last x))
    . transpose
    . map sort
    . transpose
    . intMatrix

eq :: Integer -> Integer -> Bool
eq x y = x == y

frequencyIn :: [Integer] -> Integer -> Integer
frequencyIn o x = toInteger . length . filter (eq x) $ o

frequenciesIn :: [[Integer]] -> [(Integer, Integer)]
frequenciesIn x = zip (head x) (map (frequencyIn (last x)) (head x))

similarity :: String -> Integer
similarity =
  sum
    . map (uncurry (*))
    . frequenciesIn
    . transpose
    . intMatrix

-- Day 2 --
