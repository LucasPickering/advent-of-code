{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Regex.TDFA

-- https://adventofcode.com/2020/day/2

-- parse a line like "2-4 v: vttv" into (2, 4, 'v', "vttv")
parsePasswordLine :: String -> (Int, Int, Char, String)
parsePasswordLine line =
  let rgx = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"
      -- fucking type wizardry to get match groups
      [[_, a, b, char, pw]] :: [[String]] = line =~ rgx
   in -- parse a/b, convert char from String->Char (should always be len 1)
      (read a, read b, head char, pw)

isValidPasswordPt1 :: String -> Bool
isValidPasswordPt1 pwLine =
  let (min, max, char, pw) = parsePasswordLine pwLine
      charCount = length $ filter (== char) pw
   in min <= charCount && charCount <= max

isValidPasswordPt2 :: String -> Bool
isValidPasswordPt2 pwLine =
  let (i1, i2, char, pw) = parsePasswordLine pwLine
   in -- exactly one of [pw[i1], pw[i2]] should match char
      (length $ filter (\(i, c) -> (i == i1 || i == i2) && c == char) (zip [1 .. i2] pw)) == 1

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let passwords :: [String] = lines input -- split lines
  putStrLn ("Total valid passwords [pt 1]: " ++ (show $ length $ filter isValidPasswordPt1 passwords))
  putStrLn ("Total valid passwords [pt 2]: " ++ (show $ length $ filter isValidPasswordPt2 passwords))
