{-# LANGUAGE TupleSections #-}

module Utils.Coord where

import Data.List.Split (splitOn)

type Point = (Int, Int)

type LineSegment = (Point, Point)

parsePoint :: String -> Point
parsePoint s =
  let [x, y] = map read . splitOn "," $ s
   in (x, y)

parseLineSegment :: String -> LineSegment
parseLineSegment s =
  let [p1, p2] = map parsePoint . splitOn " ->" $ s
   in (p1, p2)

isDiagonal :: LineSegment -> Bool
isDiagonal ((x1, y1), (x2, y2)) = (x1 /= x2) && (y1 /= y2)

-- | Get a list of all the points that a line segment crosses
interpolate :: LineSegment -> [Point]
interpolate ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) (range y1 y2)
  | y1 == y2 = map (,y1) (range x1 x2)
  -- Day 5 guarantees that diagonal lines are 45 degrees, meaning [x1..x2] and
  -- [y1..y2] are always the same length
  | otherwise = zip (range x1 x2) (range y1 y2)

-- | Generate a numeric range, either ascending or descending
range :: Int -> Int -> [Int]
range start end
  | start <= end = [start .. end]
  -- Range operator is annoying and needs help doing descending
  | otherwise = [start, start - 1 .. end]
