module Main where

import Lib
import Text.Printf

main :: IO ()
main = do
  day1Input <- readFileIntoInt "./files/input-day-1.txt"
  printf "Day 1A; number of increases: %d\n"  $ countIncreases day1Input
  printf "Day 1B; numberf of increases: %d\n" $ countIncreases $ sumsOfThree day1Input
  -- day2Input <- readFileIntoInstruction "./files/input-day-2.txt"
  -- printf "Day 2A; finalCoordinate: %x\n" $ fmap sequence day2Input day2Input


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

--- day 2
data Direction = Up | Forward | Down
data Instruction = Instruction Direction Int

data Coordinate = Coordinate Int Int
  deriving (Show)

x :: Coordinate -> Int
x (Coordinate x _) = x

y :: Coordinate -> Int
y (Coordinate _ y) = y
-- from 0,0 calculate the final coordinate when summing all instructions
-- where the direction is up or forward
calculateFinalCoordinate :: [Instruction] -> Coordinate
calculateFinalCoordinate = foldl (\coord (Instruction direction distance) ->
  case direction of
    Up -> Coordinate (x coord) (y coord + distance)
    Down -> Coordinate (x coord) (y coord - distance)
    Forward -> Coordinate (x coord + distance) (y coord)
  ) (Coordinate 0 0)


-- read File into a list of instructions
readFileIntoInstruction :: String -> IO [Maybe Instruction]
readFileIntoInstruction fileName = do
  contents <- readFile fileName
  return (map (parseInstruction . words) (lines contents))

parseInstruction :: [String] -> Maybe Instruction
parseInstruction [direction, distance] = Just  $ Instruction (parseDirection direction) (read distance)
parseInstruction _ = Nothing

parseDirection :: String -> Direction
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection "forward" = Forward
parseDirection _ = error "invalid direction"