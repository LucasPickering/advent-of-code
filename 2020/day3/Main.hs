{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- https://adventofcode.com/2020/day/3

-- | Summer trees, summer not
type Cell = Bool

-- | Rows and columns of a map of trees, where the rows repeat forever
-- [
--  [., #, .],
--  [#, ., .],
-- ]
type HillMap = [[Cell]]

parseTrees :: String -> HillMap
parseTrees input =
  let parseLine = cycle . map (== '#')
   in map parseLine (lines input)

countTrees :: HillMap -> (Int, Int) -> Int
countTrees [] _ = 0
-- Drop the first y rows, and the first x cells in each row
countTrees hillMap (stepX, stepY) =
  let cell = head $ head hillMap
   in fromEnum cell + countTrees (drop stepY (map (drop stepX) hillMap)) (stepX, stepY)

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let hillMap = parseTrees input
  let stepsss = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let stepToCount = map (\step -> (step, countTrees hillMap step)) stepsss
  let prod = product $ map snd stepToCount
  mapM_ print stepToCount
  putStrLn $ "Product: " ++ show prod
