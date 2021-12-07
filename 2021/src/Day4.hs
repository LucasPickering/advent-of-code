{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.List
import Data.List.Split
import Utils.Mod (filterBoth, readInputLines, readInts)

type BingoInput = [Int]

-- | A bingo board is a matrix of numbers, with a flag on each one denoting
-- | whether or not it's been marked yet
type BingoBoard = [[(Int, Bool)]]

type BingoGame = (BingoInput, [BingoBoard])

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let game = parseGame input
      winner = findFirstWinner game
      winningScore = getScore winner
  putStrLn $ "Winner: " ++ show winner
  putStrLn $ "Score: " ++ show winningScore

part2 :: IO ()
part2 = do
  input <- readInputLines
  let game = parseGame input
      lastWinner = findLastWinner game
      winningScore = getScore lastWinner
  putStrLn $ "Score: " ++ show winningScore

parseGame :: [String] -> BingoGame
parseGame [] = error "empty input"
parseGame [_] = error "empty input"
parseGame (inputLine : blank : rest) =
  let input = readInts inputLine
      -- Each of these should be an unparse board
      boards = map parseBoard . splitOn [""] $ rest
   in (input, boards)

parseBoard :: [String] -> BingoBoard
-- Split each line on whitespace, then parse each number
parseBoard = map (map ((,False) . read) . words)

-- | Run a game and get the first winner as (winning number, winning board)
findFirstWinner :: BingoGame -> (Int, BingoBoard)
findFirstWinner = findWinner 0

-- | Run a game until everyone wins to find the very last board that wins
-- | (so we can throw to the squid)
findLastWinner :: BingoGame -> (Int, BingoBoard)
-- Skip the first n-1 winners to get the last winner
findLastWinner (input, boards) = findWinner (length boards - 1) (input, boards)

-- | Find the winning board for a bingo game, skipping the first n winners.
-- | Useful for finding the first OR last winner (or any in between)
findWinner :: Int -> BingoGame -> (Int, BingoBoard)
findWinner _ ([], boards) = error $ "Game input exhausted " ++ show boards
findWinner winnersToSkip (nextNum : rest, boards)
  | winnersToSkip < 0 || winnersToSkip >= length boards = error "Invalid number of boards to skip: " winnersToSkip
  | otherwise =
    let markedBoards = markBoards nextNum boards
        -- Split boards into (ones that won in this num, everyone else)
        (winners, losers) = filterBoth isWinner markedBoards
     in -- If we've skipped enough winners that the "true" winner is in this
        -- set, then return them not. Otherwise, continue
        if winnersToSkip < length winners
          then -- Hypothetically we could get multiple winners on this number, if so
          -- just return the first one
            (nextNum, head winners)
          else findWinner (winnersToSkip - length winners) (rest, losers)

markBoards :: Int -> [BingoBoard] -> [BingoBoard]
markBoards num = map (markNumber num)

-- | Mark a number as called for a single board
markNumber :: Int -> BingoBoard -> BingoBoard
markNumber num = map (map (\(cell, marked) -> (cell, marked || cell == num)))

isWinner :: BingoBoard -> Bool
isWinner board =
  -- Check if any row is entirely marked, then swap row<==>col and do it again
  any (all snd) board || any (all snd) (transpose board)

-- | Winning score is the sum of the *unmarked* numbers in the board times the
-- | winning number
getScore :: (Int, BingoBoard) -> Int
getScore (winningNum, board) =
  -- Flatten board into a [(Int, Bool)], filter out marked cells, grab just the
  -- numbers, then sum them up
  (sum . map fst . filter (not . snd) . concat $ board) * winningNum
