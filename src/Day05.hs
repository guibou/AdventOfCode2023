{-# LANGUAGE RecordWildCards #-}
module Day05 where

import Utils
import Control.Applicative
import Text.Megaparsec (oneOf, sepBy)
import qualified Data.Map.Strict as Map
import qualified Data.Range as Range
import Data.Range ((+=+))

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
  _ <- "-to-"
  b <- some (oneOf ['a'..'z'])
  _ <- " map:\n"
  ranges <- some (parseRange <* "\n")

  pure (a, b, ranges)

parseSeed = do
  _ <- "seeds: "
  seeds <- some parseNumber
  _ <- "\n\n"
  pure (seeds :: [Int])

-- * Generics
rangeFunction :: Range -> Int -> Maybe Int
rangeFunction Range{..} v
  | v >= source && v < source + length = Just (destination + v - source)
  | otherwise = Nothing

chainRanges :: [Range] -> [Seed] -> [Seed]
chainRanges [] v = v
chainRanges (r:rx) v = do
  let (changed, unchanged) = unzip (map (rangeSeedFunction r) v)
  concat changed ++ chainRanges rx (concat unchanged)

-- * FIRST problem
solve seeds maps = do
  let paths = Map.fromList $ do
        (a, b, ranges) <- maps
        pure (a, (b, ranges))
  minimum $ map getLowerBound $ followSeedPath paths "seed" "location" seeds

day (singleSeeds -> seeds, maps) = solve seeds maps

getLowerBound (Seed (Range.SpanRange (Range.Bound a inclusion) (Range.Bound _ _))) = case inclusion of
  Range.Inclusive -> a
  Range.Exclusive -> a + 1
getLowerBound (Seed (Range.SingletonRange a)) = a
getLowerBound (Seed r) = error $ "getLowerBound: partial: " <> show r

followSeedPath paths current final value
  | current == final = value
  | otherwise = do
    let (nextStep, ranges) = paths Map.! current
    let value' = chainRanges ranges value
    followSeedPath paths nextStep final value'

-- * SECOND problem
day' (extendSeeds -> seeds, maps) = solve seeds maps

extendSeeds :: [Int] -> [Seed]
extendSeeds [] = []
extendSeeds (a:b:xs) = (Seed $ a +=+ (a + b)):extendSeeds xs
extendSeeds [_] = error "Unique seed in extendSeeds"

singleSeeds seeds = map (\s -> Seed (Range.SingletonRange s)) seeds

newtype Seed = Seed (Range.Range Int)
  deriving (Show)

rangeSeedFunction :: Range -> Seed -> ([Seed], [Seed])
rangeSeedFunction range (Seed seedRange) = do
  let transformRange = range.source +=+ (range.source + range.length - 1)
  let untouchedSeed = [seedRange] `Range.difference` [transformRange]
  let transformedRange = [seedRange] `Range.intersection` [transformRange]

  (map (translateRange (range.destination - range.source)) transformedRange, map Seed untouchedSeed)

translateRange :: Int -> Range.Range Int -> Seed
translateRange dr (Range.SpanRange (Range.Bound a _) (Range.Bound b _)) = Seed ((a + dr) +=+ (b + dr))
translateRange dr (Range.SingletonRange x) = Seed (Range.SingletonRange (x + dr))
translateRange _ r = error $ "Not handled range: " <> show r
  

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
