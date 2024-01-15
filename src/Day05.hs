{-# LANGUAGE RecordWildCards #-}
module Day05 where

import Utils
import Control.Applicative
import Text.Megaparsec (oneOf, sepBy)
import qualified Data.Map.Strict as Map

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  seeds <- parseSeed
  ranges <- parseMap `sepBy` "\n"

  pure (seeds, ranges)

data Range = Range {
  destination :: Int,
  source :: Int,
  length :: Int
 } deriving Show

parseRange = do
  destination <- parseNumber
  source <- parseNumber
  length <- parseNumber

  pure $ Range{..}

parseMap = do
  a <- some (oneOf ['a'..'z'])
  "-to-"
  b <- some (oneOf ['a'..'z'])
  " map:\n"
  ranges <- some (parseRange <* "\n")

  pure (a, b, ranges)

parseSeed = do
  "seeds: "
  seeds <- some parseNumber
  "\n"
  "\n"
  pure (seeds :: [Int])

-- * Generics
rangeFunction :: Range -> Int -> Maybe Int
rangeFunction Range{..} v
  | v >= source && v < source + length = Just (destination + v - source)
  | otherwise = Nothing

chainRanges :: [Range] -> Int -> Int
chainRanges [] v = v
chainRanges (r:rx) v = case rangeFunction r v of
  Nothing -> chainRanges rx v
  Just v -> v

-- * FIRST problem
day (seeds, maps) = do
  let paths = Map.fromList $ do
        (a, b, ranges) <- maps
        pure (a, (b, ranges))
  minimum $ map (followSeedPath paths "seed" "location") seeds

followSeedPath paths current final value
  | current == final = value
  | otherwise = do
    let (nextStep, ranges) = paths Map.! current
    let value' = chainRanges ranges value
    followSeedPath paths nextStep final value'

-- * SECOND problem
day' (seeds, maps) = day(extendSeeds seeds, maps)

extendSeeds [] = []
extendSeeds (a:b:xs) = take b [a..] <> extendSeeds xs

ex = parseContent [str|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]

-- started at Mon Jan 15 10:59:22 AM +04 2024
