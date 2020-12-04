{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import Debug.Trace
import Text.Printf
import Text.Read
import Text.Regex
import Text.Regex.PCRE

-- https://adventofcode.com/2020/day/4

-- | A passport is a list of key/value pairs
type Passport = HashMap.HashMap String String

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

fieldRgx = "([a-z]+):([\\S]+)"

isValidHeight :: String -> Bool
isValidHeight [d0, d1, d2, 'c', 'm'] = let hgt :: Int = read [d0, d1, d2] in 150 <= hgt && hgt <= 193
isValidHeight [d0, d1, 'i', 'n'] = let hgt :: Int = read [d0, d1] in 59 <= hgt && hgt <= 76
isValidHeight _ = False

-- | Check if a passport is valid
isValid :: Passport -> Bool
isValid passport =
  let byr :: Int = fromMaybe 0 (readMaybe (fromJust ("byr" `HashMap.lookup` passport)))
      iyr :: Int = fromMaybe 0 (readMaybe (fromJust ("iyr" `HashMap.lookup` passport)))
      eyr :: Int = fromMaybe 0 (readMaybe (fromJust ("eyr" `HashMap.lookup` passport)))
      hgt :: String = fromJust ("hgt" `HashMap.lookup` passport)
      hcl :: String = fromJust ("hcl" `HashMap.lookup` passport)
      ecl :: String = fromJust ("ecl" `HashMap.lookup` passport)
      pid :: String = fromJust ("pid" `HashMap.lookup` passport)
   in -- Important to do this check first, so we know the following looks are all valid
      all (`HashMap.member` passport) requiredFields
        && (1920 <= byr && byr <= 2002)
        && (2010 <= iyr && iyr <= 2020)
        && (2020 <= eyr && eyr <= 2030)
        && isValidHeight hgt
        && hcl =~ "^#[0-9a-f]{6}$"
        && ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        && pid =~ "^[0-9]{9}$"

-- | Parses a passport line into a list of key:value pairs
parsePassport :: String -> Passport
parsePassport line =
  HashMap.fromList (map (\[_, field, value] -> (field, value)) (line =~ fieldRgx :: [[String]]))

main :: IO ()
main = do
  input <- getContents -- read input from stdin
  let rawPassports = splitRegex (mkRegex "\n\n") input
  let passports = traceShowId $ map parsePassport rawPassports
  let validPassports = filter isValid passports
  printf "# valid passports: %d/%d" (length validPassports) (length passports)
