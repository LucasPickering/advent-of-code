{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.HashSet as HashSet
import Data.List.Split
import qualified Data.Map.Lazy as Map
import Debug.Trace
import Text.Printf

-- https://adventofcode.com/2020/day/6

type Group = [String]

-- | Parse lines into groups
parseGroups :: String -> [Group]
parseGroups input = splitOn [""] $ lines input

countAnswersUnion :: Group -> Int
countAnswersUnion group =
  length $ HashSet.fromList $ concat group

countAnswersIntersect :: Group -> Int
countAnswersIntersect group =
  -- Count frequency of each answer
  let groupSize = length group
      freqMap :: Map.Map Char Int = Map.fromListWith (+) (map (,1) (concat group))
      allYes = Map.filter (== groupSize) freqMap
   in length allYes

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let groups = parseGroups input
  printf "Group sum (OR): %d\n" (sum $ map countAnswersUnion groups)
  printf "Group sum (AND): %d\n" (sum $ map countAnswersIntersect groups)
