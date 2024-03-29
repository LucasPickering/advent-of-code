{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- need to update other-modules to add sub modules
import qualified Day1 (dispatch)
import qualified Day2 (dispatch)
import qualified Day3 (dispatch)
import qualified Day4 (dispatch)
import qualified Day5 (dispatch)
import qualified Day6 (dispatch)
import qualified Day7 (dispatch)
import qualified Day8 (dispatch)
-- Add day import

import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap =
  [ (1, Day1.dispatch),
    (2, Day2.dispatch),
    (3, Day3.dispatch),
    (4, Day4.dispatch),
    (5, Day5.dispatch),
    (6, Day6.dispatch),
    (7, Day7.dispatch),
    (8, Day8.dispatch)
    -- Add day dispatch
  ]

main :: IO ()
main = do
  [dayNumber, part] <- getArgs
  -- Grab the main function for the given day
  let (Just dayDispatcher) = lookup (read dayNumber) dayMap
  -- Grab the part number for that day (all days have 2 parts)
  let (Just func) = lookup (read part) dayDispatcher
  func
