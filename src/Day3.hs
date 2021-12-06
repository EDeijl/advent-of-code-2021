module Day3 where
import Data.Foldable
import Data.Ord
import Data.List

answer :: IO ()
answer = do
  bitMatrix <- readFileIntoListOfBits "./files/input-day-3.txt"
  let transposedMatrix = transpose bitMatrix
  let binGammas = gammas transposedMatrix
  let binEpsilons = epsilons transposedMatrix
  let decGamma = binToDec binGammas
  let decEpsilon = binToDec binEpsilons
  putStrLn $ "Gamma: " ++ show decGamma
  putStrLn $ "Epsilon: " ++ show decEpsilon
  putStrLn $ "Total: " ++ show (decGamma * decEpsilon)



-- collect gammas from list
gammas :: [[Int]] -> [Int]
gammas = map gamma

-- find the most common bit in list of bits
gamma :: [Int] -> Int
gamma xs = head $ maximumBy (comparing length) $ group $ sort xs

-- collect epsilons from list
epsilons :: [[Int]] -> [Int]
epsilons = map epsilon

-- find the least common bit in list of bits
epsilon :: [Int] -> Int
epsilon xs = head $ minimumBy (comparing length) $ group $ sort xs


-- binary int list to decimal
binToDec :: [Int] -> Int
binToDec = foldl (\acc x -> acc * 2 + x) 0

readFileIntoListOfBits :: String -> IO [[Int]]
readFileIntoListOfBits fileName = do
  fileContents <- readFile fileName
  return $ linesToListsOfListOfBits fileContents


linesToListsOfListOfBits :: String -> [[Int]]
linesToListsOfListOfBits input = map stringToListOfBits $ lines input


stringToListOfBits :: String -> [Int]
stringToListOfBits = map (\x -> if x == '1' then 1 else 0)