{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- need to update other-modules to add sub modules
import qualified Day1 (dispatch)
-- Add day import

import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap =
  [ (1, Day1.dispatch)
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
