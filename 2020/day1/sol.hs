import qualified Data.HashSet as HashSet
import Data.Maybe

-- https://adventofcode.com/2020/day/1

findProduct :: Int -> Int -> [Int] -> Int
findProduct n targetSum values =
  -- Use a hashset for O(1) membership lookups
  let set = HashSet.fromList values
      helper :: Int -> Int -> Maybe Int
      helper n targetSum =
        if n == 2
          then -- Grab the first satisfying pair (if any)
            listToMaybe [x * diff | x <- values, let diff = targetSum - x, diff `elem` set]
          else -- For each x in the values, find a grouping of (n-1) elements that sums to (sum-x)

            listToMaybe
              (mapMaybe (\x -> helper (n - 1) (targetSum - x)) values)
   in fromJust (helper n targetSum)

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let values = map read (lines input) -- split lines and parse to ints
  -- let pairsAnswer = getProductForPair 2020 values -- do math
  let pairsAnswer = findProduct 2 2020 values -- do math
  print ("pairs answer: " ++ show pairsAnswer)
  let triplesAnswer = findProduct 3 2020 values -- do math
  print ("triples answer: " ++ show triplesAnswer)
