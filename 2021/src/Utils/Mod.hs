{-# LANGUAGE TupleSections #-}

module Utils.Mod where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map

readInputLines :: IO [String]
readInputLines = lines <$> getContents

readInputIntLines :: IO [Int]
readInputIntLines = map read <$> readInputLines

readInts :: String -> [Int]
readInts = map read . splitOn ","

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

-- | Get the median of an integer list. If the list is an even
median :: [Int] -> Int
median nums
  -- For even length lists, we grab the middle *two* elements and average them
  | even len =
    -- a and b are the two middle elements
    let (a : b : _) = drop (len `div` 2 - 1) sorted
     in (a + b) `div` 2
  | otherwise = sorted !! len `div` 2
  where
    len = length nums
    sorted = sort nums

-- | Find the minimum value in a list, where each element is mapped via a
-- | given function before being compared. So this finds the minimum of each
-- | *output* of the mapper. Returns the original element *and* its winning
-- | (losing?) score.
minBy :: (Ord o, Show o, Show a) => (a -> o) -> [a] -> (a, o)
minBy f l =
  let answer = foldl' (minByHelper f) Nothing l
   in case answer of
        Nothing -> error "cannot get minimum of empty list"
        Just v -> v

-- | Helper for minBy, I can't figure out how to shove it into minBy. Please
-- | don't use this?
minByHelper :: (Ord o, Show o, Show a) => (a -> o) -> Maybe (a, o) -> a -> Maybe (a, o)
minByHelper f currentMin el =
  let elMapped = f el
   in case currentMin of
        Nothing -> Just (el, elMapped)
        Just (minEl, minMapped) ->
          Just
            ( if elMapped < minMapped
                then (el, elMapped)
                else (minEl, minMapped)
            )

-- | Get the summation of all values 1..n. Thanks Gauss!
summation :: Int -> Int
summation n = (n * (n + 1)) `div` 2
