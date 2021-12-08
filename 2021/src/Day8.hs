module Day8 where

import Data.List (find, foldl', sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace (traceShowId)
import Utils.Mod (frequency, invertMap, joinDigits, readInputLines, tuple)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord, Show)

data Segment = Top | TopLeft | TopRight | Middle | BottomLeft | BottomRight | Bottom
  deriving (Eq, Ord, Show)

-- A single character in a digit signal. Each character corresponds to some
-- unknown segment on the display.
type SignalWire = Char

-- A serial of segment signals, which represents one unknown digit. These are
-- sorted, for convenience later.
type SignalPattern = [SignalWire]

-- | First list is always length 10, second is always length 4
type SignalCapture = ([SignalPattern], [SignalPattern])

type SegmentDecoding = Map.Map SignalWire Segment

type DigitEncoding = Map.Map Digit SignalPattern

type DigitDecoding = Map.Map SignalPattern Digit

allDigits = [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

-- | Digits that have a unique number of segments
easyDigits = [One, Four, Seven, Eight]

segmentFrequencies :: Map.Map Segment Int
segmentFrequencies = frequency . concatMap (Set.toList . digitToSegments) $ allDigits

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

part1 :: IO ()
part1 = do
  input <- readInputLines
  let captures = parseInput input
      -- The number of segments for each digit that has a unique number of segments
      easySignalLengths = map segmentCount easyDigits
      -- Count how many times each "easy" digit appears in all outputs
      easySignals = filter ((`elem` easySignalLengths) . length) . concatMap snd $ captures
  putStrLn $ "Easy digits: " ++ show (length easySignals)

part2 :: IO ()
part2 = do
  input <- readInputLines
  let captures = parseInput input
      decodedValues = map decodeCapture captures
      answer = sum decodedValues
  putStrLn $ "Values: " ++ show decodedValues
  putStrLn $ "Sum: " ++ show answer

-- | Parse each input line. Each one looks like:
-- | be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
-- | 10 patterns | 4 patterns
-- | We want to sort each of these groupings to be alphabetical, to make
-- | comparisons easy later.
parseInput :: [String] -> [SignalCapture]
parseInput = map (tuple . splitOn ["|"] . sort . words)

-- | Decode a signal capture into its output value
decodeCapture :: SignalCapture -> Int
decodeCapture (signals, output) =
  let decoding = decodeSignals signals
      digits = map (digitToInt . fromJust . (`Map.lookup` decoding)) output
   in joinDigits digits

decodeSignals :: [SignalPattern] -> DigitDecoding
decodeSignals signals =
  -- Get the easy digits first. These will be a foundation that we get the rest from
  let digitDecoding = Map.fromList . map (\digit -> (findByLength digit signals, digit)) $ easyDigits
      digitEncoding = invertMap digitDecoding
      lookUpDigit :: Digit -> SignalPattern
      lookUpDigit = fromJust . (`Map.lookup` digitEncoding)
      oneSegments = lookUpDigit One
      fourSegments = lookUpDigit Four
      sevenSegments = lookUpDigit Seven
      eightSegments = lookUpDigit Eight

      -- Find a digit within the unknown signals by subtracting away all the
      -- segments from one or more known digits, until the number of remaining
      -- digits matches some expected and unique value. For example, once we
      -- know the patterns for 4 and 7, we can identify 0 by subtracting away
      -- the segments of 4 and 7 from each signal pattern until we find one
      -- with exactly one segment remaining (which would be the top-left).
      findByDifference :: Int -> [SignalPattern] -> Maybe SignalPattern
      findByDifference expectedLen subtractors = find (\signal -> (==) expectedLen . length . foldl' listDifference signal $ subtractors) signals

      zero = fromJust $ findByDifference 1 [fourSegments, sevenSegments]

      segmentDecoding = decodeSegments signals digitDecoding
      segments = map (`Map.lookup` segmentDecoding)
   in ()

-- | Using a *partial* digit decoding, this figures out a complete mapping of
-- | wire->segment using some deductive reasoning. The input digit decoding
-- | should have the four easy digits (1, 4, 7, 8) in it to enable this.
decodeSegments :: [SignalPattern] -> DigitDecoding -> SegmentDecoding
decodeSegments signals digitDecoding =
  let digitEncoding = invertMap digitDecoding
      lookUpDigit = fromJust . (`Map.lookup` digitEncoding)
      -- Count how many times each signal character appears
      signalFrequencies = frequency . concat $ signals
      freqLookup = fromJust . (`Map.lookup` signalFrequencies)
      oneSegments = lookUpDigit One
      fourSegments = lookUpDigit Four
      sevenSegments = lookUpDigit Seven
      eightSegments = lookUpDigit Eight
      -- The top segment is the only one that is in 7 but not 1
      [top] = sevenSegments `listDifference` oneSegments
      -- A 1 consists of [TR, BR]. We know TR should appear 8 times among all
      -- digits, and BR 9 times. So we sort the two by their frequncy among all
      -- digits
      [topRight, bottomRight] = sortOn freqLookup oneSegments
      -- Bottom left and bottom appaer in 8, but not 7 or 4. And BL appears
      -- 5 times among all digits while B appears 7, so use that to figure out
      -- which is which
      [bottomLeft, bottom] = sortOn freqLookup (eightSegments `listDifference` fourSegments `listDifference` sevenSegments)
      -- Now we can identify 0 using the five segments we know. There are three
      -- numbers that use six segments (0, 6, 9), but only 0 uses the five
      -- segments we already know (T,TR,BR,B,BL)
      minusKnownFive = map (\sigs -> (sigs, sigs `listDifference` [top, topRight, bottomRight, bottomLeft, bottom])) signals
      -- 0 is the number that has exactly one signal left after removing the known 5
      -- The associated singular segment is the code for TL
      (zero, [topLeft]) = fromJust $ find ((==) 1 . length . snd) minusKnownFive
      -- Middle is the last one left
      [middle] = eightSegments `listDifference` [top, topLeft, topRight, bottomLeft, bottomRight, bottom]
   in Map.fromList
        [ (top, Top),
          (topLeft, TopLeft),
          (topRight, TopRight),
          (bottomLeft, BottomLeft),
          (bottomRight, BottomRight),
          (bottom, Bottom)
        ]

-- | Find a digit in the list of signals solely by its length. Expects exactly
-- | one signal to match that length, otherwise an error occurs.
findByLength :: Digit -> [SignalPattern] -> SignalPattern
findByLength digit signals =
  let targetLen = segmentCount digit
      [answer] = filter ((== targetLen) . length) signals
   in answer

-- | How many segments does each digit require?
segmentCount :: Digit -> Int
segmentCount = length . digitToSegments

-- | Which segments are needed to light up a particular digit?
digitToSegments :: Digit -> Set.Set Segment
digitToSegments Zero = Set.fromList [Top, TopLeft, TopRight, BottomLeft, BottomRight, Bottom]
digitToSegments One = Set.fromList [TopRight, BottomRight]
digitToSegments Two = Set.fromList [Top, TopLeft, Middle, BottomRight, Bottom]
digitToSegments Three = Set.fromList [Top, TopRight, Middle, BottomRight, Bottom]
digitToSegments Four = Set.fromList [TopLeft, TopRight, Middle, BottomRight]
digitToSegments Five = Set.fromList [Top, TopRight, Middle, BottomLeft, Bottom]
digitToSegments Six = Set.fromList [Top, TopLeft, Middle, BottomLeft, BottomRight, Bottom]
digitToSegments Seven = Set.fromList [Top, TopRight, BottomRight]
digitToSegments Eight = Set.fromList [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom]
digitToSegments Nine = Set.fromList [Top, TopLeft, TopRight, Middle, BottomRight, Bottom]

digitToInt :: Digit -> Int
digitToInt Zero = 0
digitToInt One = 1
digitToInt Two = 2
digitToInt Three = 3
digitToInt Four = 4
digitToInt Five = 5
digitToInt Six = 6
digitToInt Seven = 7
digitToInt Eight = 8
digitToInt Nine = 9

listDifference :: Ord a => [a] -> [a] -> [a]
listDifference l1 l2 = Set.toList (Set.fromList l1 `Set.difference` Set.fromList l2)
