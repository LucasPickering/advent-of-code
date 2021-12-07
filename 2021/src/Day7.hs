module Day7 where

import Utils.Mod (median, minBy, readInputLines, readInts, summation)

type Position = Int

type Crabs = [Position]

type Fuel = Int

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let crabs = parseInput input
      (position, fuel) = findCheapestDestination crabs
  putStrLn $ "Position: " ++ show position
  putStrLn $ "Fuel: " ++ show fuel

part2 :: IO ()
part2 = do
  input <- readInputLines
  let crabs = parseInput input
      (position, fuel) = findCheapestDestination2 crabs
  putStrLn $ "Position: " ++ show position
  putStrLn $ "Fuel: " ++ show fuel

parseInput :: [String] -> Crabs
parseInput [line] = readInts line
parseInput other = error $ "Invalid input: " ++ show other

-- | Find the cheapest location for all crabs to move to, in terms of total
-- | fuel expended (for part 1)
findCheapestDestination :: Crabs -> (Position, Fuel)
findCheapestDestination crabs =
  let position = median crabs
   in (position, fuelToPositionAll fuelCostLinear crabs position)

-- | Count the total fuel required for all crabs to move to a given position.
-- | Takes in a function that determines fuel cost, for genericalism.
fuelToPositionAll :: (Position -> Position -> Fuel) -> Crabs -> Position -> Fuel
fuelToPositionAll f crabs dest = sum . map (f dest) $ crabs

-- | How much does it cost to move a->b? (part 1)
fuelCostLinear :: Position -> Position -> Fuel
fuelCostLinear start dest = abs (dest - start)

-- | Find the cheapest location for all crabs to move to, in terms of total
-- | fuel expended (for part 2). Part 2 uses a more complicated fuel formula.
findCheapestDestination2 :: Crabs -> (Position, Fuel)
findCheapestDestination2 crabs =
  -- We want to check every position between all the crabs, it's possible that
  -- the answer is a position that no crab is on to begin with
  let firstCrab = minimum crabs
      lastCrab = maximum crabs
   in minBy (fuelToPositionAll fuelCostIncreasing crabs) [firstCrab .. lastCrab]

-- | How much does it cost to move a->b? (part 2)
fuelCostIncreasing :: Position -> Position -> Fuel
fuelCostIncreasing start dest = summation . abs $ (dest - start)
