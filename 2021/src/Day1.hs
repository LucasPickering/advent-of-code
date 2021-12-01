module Day1 where

import Utils.Mod (readInputInts, windows)

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  depths <- readInputInts
  let increases = countIncreases depths
  putStrLn $ "Increases: " ++ show increases

part2 :: IO ()
part2 = do
  depths <- readInputInts
  let sums = sumTriplets depths
  let increases = countIncreases sums
  putStrLn $ "Increases: " ++ show increases

countIncreases :: [Int] -> Int
countIncreases xs = length $ filter (\[a, b] -> a < b) (windows 2 xs)

sumTriplets :: [Int] -> [Int]
sumTriplets xs = map sum $ windows 3 xs
