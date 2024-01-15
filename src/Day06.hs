module Day06 where

import Utils
import Control.Applicative

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  symbol "Time:"
  times <- some parseNumber
  "\n"
  symbol "Distance:"
  distances <- some parseNumber
  "\n"
  pure (zip times distances)

-- * Generics
raceDistance time = map (\t -> (t, t * (time - t))) [0..time]

scoreRace (time, record) = length $ filter (\(t, d) -> d > record) (raceDistance time)

-- * FIRST problem
day races = product $ map scoreRace races

-- * SECOND problem
day' (unzip -> (times, records)) = do
  let t = read $ concatMap show times
  let r = read $ concatMap show records
  scoreRace (t, r)

ex = parseContent [str|Time:      7  15   30
Distance:  9  40  200
|]

-- started at Mon Jan 15 01:13:41 PM +04 2024
