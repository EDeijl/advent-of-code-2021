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
  let oxygenRating = binToDec $ getOxygenRating bitMatrix
  let scrubberRating = binToDec $ getScrubberRating bitMatrix
  let totalRating = oxygenRating * scrubberRating
  putStrLn $ "Gamma: " ++ show decGamma
  putStrLn $ "Epsilon: " ++ show decEpsilon
  putStrLn $ "Total: " ++ show (decGamma * decEpsilon)
  putStrLn $ "Oxygen rating: " ++ show oxygenRating
  putStrLn $ "Scrubber rating: " ++ show scrubberRating
  putStrLn $ "Total rating: " ++ show totalRating


getOxygenRating :: [[Int]] -> [Int]
getOxygenRating matrix = head $ gammaReducer 0 matrix (head (transpose matrix))

getScrubberRating :: [[Int]] -> [Int]
getScrubberRating matrix = head $ epsilonReducer 0 matrix (head (transpose matrix))

gammaReducer :: Int -> [[Int]] -> [Int] -> [[Int]]
gammaReducer _ [] _ = []
gammaReducer _ [x] _ = [x]
gammaReducer i matrix gammaBits = gammaReducer (i+1) filtered (transpose filtered !! (i+1))
  where
    filtered = filterMatrixByGammaAtN matrix i (gamma gammaBits)

epsilonReducer :: Int -> [[Int]] -> [Int] -> [[Int]]
epsilonReducer _ [] _ = []
epsilonReducer _ [x] _ = [x]
epsilonReducer i matrix epsilonBits = epsilonReducer (i+1) filtered (transpose filtered !! (i+1))
  where
    filtered = filterMatrixByEpsilonAtN matrix i (epsilon epsilonBits)

filterMatrixByEpsilonAtN :: [[Int]] -> Int -> Int -> [[Int]]
filterMatrixByEpsilonAtN matrix n epsilon =  filter (\x -> x !! n == epsilon) matrix

o2genr :: Int -> [[Int]] -> [[Int]]
o2genr n matrix = filter (\x -> x !! n == gamma (transpose matrix !! n)) matrix

-- run a mapping function zipped with an index
mapWithIndex :: (Int -> [Int] -> [Int]) -> [[Int]] -> [[Int]]
mapWithIndex f = zipWith f [0..]


gammaAtN :: [[Int]] -> Int -> Int
gammaAtN matrix n = gamma $ matrix !! n

filterMatrixByGammaAtN :: [[Int]] -> Int -> Int -> [[Int]]
filterMatrixByGammaAtN matrix n gamma = filter (\x -> x !! n == gamma) matrix

hasGammaAtPositionN :: [Int] -> Int -> Int -> Bool
hasGammaAtPositionN row n gamma = row !! n == gamma



filterGamma :: [Int] -> [Int]
filterGamma l@(x:xs) = if x == gamma l then l else []
filterGamma _ = []

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

-- utilities

readFileIntoListOfBits :: String -> IO [[Int]]
readFileIntoListOfBits fileName = do
  fileContents <- readFile fileName
  return $ linesToListsOfListOfBits fileContents

-- convert a string of lines to a list of lists of bits
linesToListsOfListOfBits :: String -> [[Int]]
linesToListsOfListOfBits input = map stringToListOfBits $ lines input

-- convert a string to a list of bits
stringToListOfBits :: String -> [Int]
stringToListOfBits = map (\x -> if x == '1' then 1 else 0)