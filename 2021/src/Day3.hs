module Day3 where

import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.List (transpose)
import Utils.Mod (readInputLines)

type Bit = Int

type BitString = [Bit]

type BitColumn = [Bit]

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let bs = linesToBitstrings input
      (gamma, epsilon) = gammaEpsilon bs
  putStrLn $ "Gamma: " ++ show gamma ++ "; Epsilon: " ++ show epsilon
  putStrLn $ "Answer: " ++ show (gamma * epsilon)

part2 :: IO ()
part2 = do
  input <- readInputLines
  let bs = linesToBitstrings input
      oxygen = oxygenRating bs
      co2 = co2Rating bs
  putStrLn $ "Oxygen: " ++ show oxygen ++ "; CO2: " ++ show co2
  putStrLn $ "Answer: " ++ show (oxygen * co2)

linesToBitstrings :: [String] -> [BitString]
linesToBitstrings = map (map digitToInt)

bitstringToInt :: BitString -> Int
bitstringToInt = foldl (\acc bit -> acc * 2 + bit) 0

gammaEpsilon :: [BitString] -> (Int, Int)
gammaEpsilon bitstrings =
  let (mostCommon, leastCommon) = mostLeastCommon bitstrings
   in (bitstringToInt mostCommon, bitstringToInt leastCommon)

oxygenRating :: [BitString] -> Int
oxygenRating bitstrings = findMatchingRating "oxygen" bitstrings 0

co2Rating :: [BitString] -> Int
co2Rating bitstrings = findMatchingRating "co2" bitstrings 0

findMatchingRating :: String -> [BitString] -> Int -> Int
findMatchingRating _ [final] _ = bitstringToInt final
findMatchingRating rating bitstrings index =
  let bitCriteria = getBitCriteria rating (countBits bitstrings !! index)
   in findMatchingRating rating (filter (\bitstring -> bitstring !! index == bitCriteria) bitstrings) (index + 1)

getBitCriteria :: String -> (Int, Int) -> Int
getBitCriteria "oxygen" (zeroes, ones)
  | zeroes > ones = 0
  | otherwise = 1
getBitCriteria "co2" (zeroes, ones)
  | zeroes <= ones = 0
  | otherwise = 1
getBitCriteria rating _ = error $ "Unknown rating: " ++ rating

mostLeastCommon :: [BitString] -> (BitString, BitString)
mostLeastCommon bitstrings =
  let countsByCol = countBits bitstrings
      mostCommon = map (\(numZeroes, numOnes) -> if numOnes > numZeroes then 1 else 0) countsByCol
   in (mostCommon, invert mostCommon)

-- | Invert a bitstring
invert :: BitString -> BitString
invert = map (1 -)

-- | Count the number of (zeroes, ones) at each position in the list of bitstrings.
-- | This assumes each bitstring in the given list has the same length, and the
-- | output will have that length as well.
countBits :: [BitString] -> [(Int, Int)]
-- Swap rows<-->columns, then sum up each column
countBits bitstrings =
  let numRows = length bitstrings
   in map (\col -> let numOnes = sum col in (numRows - numOnes, numOnes)) (transpose bitstrings)

-- | Arrange a tuple so the bigger number is first
greaterFirst :: (Int, Int) -> (Int, Int)
greaterFirst (a, b) = if a >= b then (a, b) else (b, a)
