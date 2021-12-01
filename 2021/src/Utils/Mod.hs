module Utils.Mod where

import Data.List

readInputLines :: IO [String]
readInputLines = lines <$> getContents

readInputInts :: IO [Int]
readInputInts = map read <$> readInputLines

-- Iterate over a list with sliding windows of a fixed size. This will never
-- include windows smaller than the specified size, so generally there are
-- (length - n + 1) windows
windows :: Int -> [a] -> [[a]]
-- This solution gives you dwindling windows at the end, so we want to filter those out
windows n xs = filter ((== n) . length) (Data.List.transpose (take n (tails xs)))
