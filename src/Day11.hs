module Day11 where

import Utils
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (inits, tails)
import Debug.Trace (traceShowId)

fileContent = parseContent $(getFile)

parseContent t = do
  let galaxies = Set.fromList $ map fst $ filter (\(_pos, c) -> c == '#') $ Map.toList $ parse2DGrid (id @Char) t
  galaxies

-- * Generics
expand expansion galaxies = do
  let used_rows = Set.map (\(V2 _ y) -> y) galaxies
  let used_columns = Set.map (\(V2 x _) -> x) galaxies

  let unused_rows = traceShowId $ Set.fromList [minimum used_rows .. maximum used_rows] `Set.difference` used_rows
  let unused_columns = traceShowId $ Set.fromList [minimum used_columns .. maximum used_columns] `Set.difference` used_columns

  let expandAGalaxy (V2 x y) = V2 (x + expandedX * (expansion - 1)) (y + expandedY * (expansion - 1))
       where
         expandedX = length $ fst $ Set.split x unused_columns
         expandedY = traceShowId $ length $ fst $ Set.split y unused_rows

  Set.map expandAGalaxy galaxies

-- * FIRST problem
computeDistances expansion galaxies = sum $ do
   (a:bs) <- tails $ Set.toList $ expand expansion galaxies
   b <- bs

   pure $ manDistance a b

manDistance (V2 x y) (V2 x' y') = abs (x - x') + abs (y - y')

day = computeDistances 2
day' = computeDistances 1000000

-- * SECOND problem

ex = parseContent [str|\
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|]

-- started at Wed Dec 25 10:41:18 PM +04 2024
