module Day5 where

import Data.List.Split

answer :: IO ()
answer = do
  putStrLn "Day 5"
  input <- readFile "./files/input-day-5.txt"
  let instructions = parseInput input
  let validInstructions = filter (\i -> x (start i) == x (end i) || y (start i) == y (end i)) instructions
  let numberOfIntersections = crossMapAllIntersects validInstructions

  putStrLn $ "Part 1 numberOfIntersections: " ++ show numberOfIntersections

data Point = Point {
    x :: Int
  , y :: Int
} deriving (Show, Eq)

data Vector = Vector {
    start :: Point
  , end :: Point
} deriving (Show, Eq)

data Direction = Vertical | Horizontal deriving (Show, Eq)

crossMapAllIntersects :: [Vector] -> Int
crossMapAllIntersects vectors = foldl (\acc vector -> acc + length (filter id (intersects vector vectors))) 0 vectors

-- check if any vector in the list interects with any other vector
intersects :: Vector -> [Vector] -> [Bool]
intersects v = map (\vec -> (vec /= v) && doIntersect v vec)

-- Funky math times

-- Check if 2 vectors intersect
doIntersect :: Vector -> Vector -> Bool
doIntersect vec1 vec2 =  (o1 /= o2 && o3 /= o4)
                      || (o1 == 0 && onSegment (start vec1) (start vec2) (end vec1))
                      || (o2 == 0 && onSegment (start vec1) (end vec2) (end vec1))
                      || (o3 == 0 && onSegment (start vec2) (start vec1) (end vec2))
                      || (o4 == 0 && onSegment (start vec2) (end vec1) (end vec2))
  where
    o1 = orientation (start vec1) (end vec1) (start vec2)
    o2 = orientation (start vec1) (end vec1) (end vec2)
    o3 = orientation (start vec2) (end vec2) (start vec1)
    o4 = orientation (start vec2) (end vec2) (end vec1)

-- check orietentation of 3 points
-- 0 -> colinear
-- 1 -> clockwise
-- 2 -> counterclockwise
orientation :: Point -> Point -> Point -> Int
orientation p q r
  | det == 0 = 0
  | det > 0 =  1
  | otherwise = 2
  where
    det = (y q - y p) * (x r - x q) - (x q - x p) * (y r - y q)

-- given three points p, q, r check if q lies on segment pr
onSegment :: Point -> Point -> Point -> Bool
onSegment p q r =  (x q <= max (x p) (x r)) && (x q >= min (x p) (x r))
                && (y q <= max (y p) (y r)) && (y q >= min (y p) (y r))

-- Data parsing

parseInput :: String -> [Vector]
parseInput input = map parseVector $ lines input

-- Brute force it yo
vectorToList :: Vector -> [Int]
vectorToList v = replicate (vectorLength v) 1

vectorToGrid :: Int -> Int -> Vector -> [[Int]]
vectorToGrid width height v =
  case vectorDirection v of
    Vertical -> 
    Horizontal -> concat [replicate (y start v) 0, row, ]
  where
    points = vectorToList v
    row = concat [replicate (x (start v)) 0, points, replicate (width - x (end v)) 0]
    column = concat [replicate (y start v)) 0, points, replicate (height - y (end v)) 0]

-- Parse a string like "0,9 -> 5,9" into a Vector (Point 0 9) (Point 5 9)
parseVector :: String -> Vector
parseVector s = Vector (Point (read x1) (read y1)) (Point (read x2) (read y2))
  where
    [x1, y1, _, x2, y2] = concatMap (splitOn ",") (words s)


vectorLength :: Vector -> Int
vectorLength v
  | vectorDirection v == Vertical   = y (start v') + y (end v') + 1
  | otherwise  = x (start v') + x (end v') + 1 
  where
    v' = normalizeVector v


vectorDirection :: Vector -> Direction
vectorDirection v = if x (start v) == x (end v) then Vertical else Horizontal

normalizeVector :: Vector -> Vector
normalizeVector v
  | x (start v) > x (end v) = Vector (end v) (start v)
  | y (start v) > y (end v) = Vector (end v) (start v)
  | otherwise = v