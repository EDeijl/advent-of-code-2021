module Day4 where

import Data.List.Split (splitOn)
import Data.List
-- Constructing the game data
type Inputs = [Int]
type Row = [Cell]

data Board = Board {
    hasWon :: Bool
  , rows :: [Row]
} deriving (Eq)

instance Show Board where
  show (Board hasWon rows) = "HasWon: " ++ show hasWon ++ "\n" ++ unlines (map show rows)

data Cell = Cell {
    value :: Int
  , marked :: Bool
} deriving (Show, Eq)


answer :: IO ()
answer = do
  input <- readFile "./files/input-day-4.txt"
  let (inputs, boards) = parseStringToGame input
  let (finalNumber, winningBoard) = play inputs boards
  print (finalNumber, winningBoard)
  putStrLn $ "Score of winning board: " ++ show (finalNumber * scoreBoard winningBoard)


scoreBoard :: Board -> Int
scoreBoard board = sum $ map value (filter (not . marked) (concat (rows board)))



play :: Inputs -> [Board] -> (Int, Board)
play [] boards = error "no winners"
play (x:xs) boards =
  let nextBoards = map (playBingoStep x) boards
      maybeWinningBoard = find hasWon nextBoards
  in case maybeWinningBoard of
        Just winningBoard -> (x, winningBoard)
        Nothing -> play xs nextBoards



playBingoStep :: Int -> Board -> Board
playBingoStep v board =
  let newRows = map (map (markCellIfValue v)) (rows board)
      hasBingo' = any hasBingo (newRows ++ transpose newRows)
  in
    board { rows = newRows, hasWon = hasBingo' }



-- Check if a row has a bingo
hasBingo :: Row -> Bool
hasBingo = all marked


markCellIfValue :: Int -> Cell -> Cell
markCellIfValue v cell = if v == value cell then Cell { value = v, marked = True } else cell


-- Should parse a multiline string seperated with a blank line into a board
parseStringToGame :: String -> (Inputs, [Board])
parseStringToGame input = do
  let inputs = parseStringToInputs (head (lines input))
  let boards = map parseStringToBoard $ drop 1 $ splitOn "\n\n" input
  (inputs, boards)

parseStringToInputs :: String -> Inputs
parseStringToInputs input = map read $ splitOn "," input

parseStringToBoard :: String -> Board
parseStringToBoard input = Board { hasWon = False , rows = map parseStringToRow $ lines input }

parseStringToRow :: String -> Row
parseStringToRow input = map parseStringToCell (words input)

parseStringToCell :: String -> Cell
parseStringToCell input = Cell {
    value = read input
  , marked = False
}
