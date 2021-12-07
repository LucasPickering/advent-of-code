{-# LANGUAGE ScopedTypeVariables #-}

module Day6 where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace
import Utils.Mod (frequency, readInputLines, readInts)

-- | A fixed-length list representing how many lanternfish will spawn in various
-- | numbers of days. Length is always 9, where index 0 is how many fish spawn
-- | in 0 days, index 1 in 1 days, ... up to index 8 in 8 days
type LanternfishCalendar = [Int]

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = main 80

part2 :: IO ()
part2 = main 256

main :: Int -> IO ()
main days = do
  input <- readInputLines
  let fish = parseInput input
      moreFish = simulateDays days fish
      count = countFish moreFish
  putStrLn $ "# of fish: " ++ show count

parseInput :: [String] -> LanternfishCalendar
-- Input is a single line of comma-separated values
parseInput [line] =
  let fishes = readInts line
      calendarMap = frequency fishes
   in -- Generate a list with one element per calendar day, and pre-populate it
      map (fromMaybe 0 . (`Map.lookup` calendarMap)) [0 .. 8]
parseInput other = error $ "Invalid input: " ++ show other

-- | Run the simulation for some number of days
simulateDays :: Int -> LanternfishCalendar -> LanternfishCalendar
simulateDays days fishes = foldl' (\acc _ -> simulateDay acc) fishes [0 .. days - 1]

-- | Advance the simulation one day
simulateDay :: LanternfishCalendar -> LanternfishCalendar
-- Hell yeah I did this and you can't stop me
simulateDay [zero, one, two, three, four, five, six, seven, eight] =
  -- First element is all the fish that are ready to spawn. Move those back to
  -- day 6, then duplicate them on day 8
  [one, two, three, four, five, six, seven + zero, eight, zero]
simulateDay calendar = error $ "unexpected calendar: " ++ show calendar

countFish :: LanternfishCalendar -> Int
countFish = sum
