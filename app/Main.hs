module Main where

import Lib
import Day1
import Day2
import Text.Printf
import Data.List
import Data.Ord
import Data.Bits (Bits(rotateL))
import Day3 (answer)
import Day4 (answer)

main :: IO ()
main = do
  day1Input <- readFileIntoInt "./files/input-day-1.txt"
  printf "Day 1A; number of increases: %d\n"  $ countIncreases day1Input
  printf "Day 1B; numberf of increases: %d\n" $ countIncreases $ sumsOfThree day1Input
  day2Input <- readFileIntoInstruction "./files/input-day-2.txt"
  printf "Day 2; finalCoordinate: %d\n" $ multCoord $ calculateFinalCoordinate day2Input
  Day3.answer
  Day4.answer