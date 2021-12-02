module DayX where

import Utils.Mod (readInputLines)

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  putStrLn $ "Answer: " ++ show 42

part2 :: IO ()
part2 = do
  input <- readInputLines
  putStrLn $ "Answer: " ++ show 42
