module Day1 where

--- Day 1
-- return number of times a number is larger than the previous number in a list
countIncreases :: [Int] -> Int
countIncreases (x:y:xs) = if y > x then 1 + countIncreases (y:xs) else countIncreases (y:xs)
countIncreases _ = 0


-- for every three items in a list compute the sum of the items
sumsOfThree :: [Int] -> [Int]
sumsOfThree (x:y:z:xs) = (x + y + z) : sumsOfThree (y:z:xs)
sumsOfThree _ = []


-- read from file every word into an integer
readFileIntoInt :: String -> IO [Int]
readFileIntoInt fileName = do
  contents <- readFile fileName
  return (map read (words contents))