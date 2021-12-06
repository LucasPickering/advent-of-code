{-# LANGUAGE TupleSections #-}

module Utils.Mod where

import Data.List
import qualified Data.Map as Map

readInputLines :: IO [String]
readInputLines = lines <$> getContents

readInputIntLines :: IO [Int]
readInputIntLines = map read <$> readInputLines

-- Iterate over a list with sliding windows of a fixed size. This will never
-- include windows smaller than the specified size, so generally there are
-- (length - n + 1) windows
windows :: Int -> [a] -> [[a]]
-- This solution gives you dwindling windows at the end, so we want to filter those out
windows n xs = filter ((== n) . length) (Data.List.transpose (take n (tails xs)))

-- | Filter a list, but return both the matches *and* the non-matches, rather
-- | than throwing the non-matches away. Returns (matches, non-matches)
filterBoth :: (a -> Bool) -> [a] -> ([a], [a])
filterBoth predicate = foldr (\e (as, bs) -> if predicate e then (e : as, bs) else (as, e : bs)) ([], [])

-- | Count the number of occurrences of each value in a list
frequency :: Ord k => [k] -> Map.Map k Int
frequency values = Map.fromListWith (+) (map (,1) values)
