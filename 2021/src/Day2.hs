module Day2 where

import Utils.Mod (readInputLines)

type Move = (String, Int)

-- | (horizontal, depth)
type SubPosSimple = (Int, Int)

-- | (horizontal, depth, aim)
type SubPos = (Int, Int, Int)

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let moves = map parseMove input
  let pos = foldl applyMoveSimple (0, 0) moves
  putStrLn $ "Final pos: " ++ show pos
  let (h, d) = pos
  putStrLn $ "Answer: " ++ show (h * d)

part2 :: IO ()
part2 = do
  input <- readInputLines
  let moves = map parseMove input
  let pos = foldl applyMove (0, 0, 0) moves
  putStrLn $ "Final pos: " ++ show pos
  let (h, d, _) = pos
  putStrLn $ "Answer: " ++ show (h * d)

parseMove :: String -> Move
parseMove input =
  let [direction, distance] = words input
   in (direction, read distance)

applyMoveSimple :: SubPosSimple -> Move -> SubPosSimple
applyMoveSimple (horizontal, depth) ("forward", distance) = (horizontal + distance, depth)
applyMoveSimple (horizontal, depth) ("down", distance) = (horizontal, depth + distance)
applyMoveSimple (horizontal, depth) ("up", distance) = (horizontal, depth - distance)
applyMoveSimple _ (direction, _) = error ("invalid direction: " ++ direction)

applyMove :: SubPos -> Move -> SubPos
applyMove (horizontal, depth, aim) ("forward", amount) = (horizontal + amount, depth + (aim * amount), aim)
applyMove (horizontal, depth, aim) ("down", amount) = (horizontal, depth, aim + amount)
applyMove (horizontal, depth, aim) ("up", amount) = (horizontal, depth, aim - amount)
applyMove _ (direction, _) = error ("invalid direction: " ++ direction)
