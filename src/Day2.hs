module Day2 where
--- day 2
data Direction = Up | Forward | Down
data Instruction = Instruction Direction Int

data Coordinate = Coordinate Int Int Int
  deriving (Show)

x :: Coordinate -> Int
x (Coordinate x _ _) = x

y :: Coordinate -> Int
y (Coordinate _ y _) = y

z :: Coordinate -> Int
z (Coordinate _ _ z) = z

multCoord :: Coordinate -> Int
multCoord (Coordinate x y _) = x * y
-- from 0,0 calculate the final coordinate when summing all instructions
-- where the direction is up or forward
calculateFinalCoordinate :: [Instruction] -> Coordinate
calculateFinalCoordinate = foldl (\coord (Instruction direction distance) ->
  case direction of
    Up -> Coordinate (x coord) (y coord) (z coord - distance)
    Down -> Coordinate (x coord) (y coord) (z coord + distance)
    Forward -> Coordinate (x coord + distance) (y coord + (z coord * distance)) (z coord)
  ) (Coordinate 0 0 0)


-- read File into a list of instructions
readFileIntoInstruction :: String -> IO [Instruction]
readFileIntoInstruction fileName = do
  contents <- readFile fileName
  return (map (parseInstruction . words) (lines contents))

parseInstruction :: [String] -> Instruction
parseInstruction [direction, distance] =  Instruction (parseDirection direction) (read distance)
parseInstruction _ = error "something went horribly wrong parsing instructions"

parseDirection :: String -> Direction
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection "forward" = Forward
parseDirection _ = error "invalid direction"