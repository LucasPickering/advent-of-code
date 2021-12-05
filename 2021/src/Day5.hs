module Day5 where

import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')
import Debug.Trace
import Utils.Coord (LineSegment, Point, interpolate, isDiagonal, parseLineSegment)
import Utils.Mod (readInputLines)

-- | A *sparse* mapping of point -> number of vents on that point. A point with
-- | zero vents will be absent from the map.
type Grid = HashMap.HashMap Point Int

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let lineSegments = filter (not . isDiagonal) . parseInput $ input
      -- Remove diagonal lines, because that's what the prompt says
      grid = populateGrid lineSegments
      yikes = countYikes grid
  putStrLn $ "Answer: " ++ show yikes

part2 :: IO ()
part2 = do
  input <- readInputLines
  let lineSegments = parseInput input
      grid = populateGrid lineSegments
      yikes = countYikes grid
  putStrLn $ "Answer: " ++ show yikes

parseInput :: [String] -> [LineSegment]
parseInput = map parseLineSegment

populateGrid :: [LineSegment] -> Grid
populateGrid lines =
  let points = concatMap interpolate lines
   in HashMap.fromListWith (+) [(p, 1) | p <- points]

countYikes :: Grid -> Int
countYikes = length . HashMap.filter (> 1)
