import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf

-- https://adventofcode.com/2020/day/5

type Seat = (Int, Int)

seatId :: Seat -> Int
seatId (row, col) = row * 8 + col

pivot :: Int -> Int -> Int
pivot lo hi = (hi - lo + 1) `div` 2 + lo

binSplit :: Char -> (Int, Int) -> (Int, Int)
binSplit c (lo, hi) | c `elem` "FL" = (lo, pivot lo hi)
binSplit c (lo, hi) | c `elem` "BR" = (pivot lo hi, hi)

binSearchHelper :: String -> (Int, Int) -> Int
binSearchHelper [] (lo, _) = lo -- lo == hi here
binSearchHelper (dir : path) lohi = binSearchHelper path (binSplit dir lohi)

parseSeat :: String -> Seat
parseSeat binString =
  let (rowPath, colPath) = splitAt 7 binString
   in (binSearchHelper rowPath (0, 127), binSearchHelper colPath (0, 7))

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let seatIds = map (seatId . parseSeat) (lines input)
      seatSet = HashSet.fromList seatIds
      minSeat = minimum seatIds
      maxSeat = maximum seatIds
      mySeat = fromJust $ find (not . (`elem` seatSet)) [minSeat .. maxSeat]
  printf "Highest seat: %d\n" maxSeat
  printf "My seat: %d\n" mySeat
