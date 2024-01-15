module Day02 where

import Utils
import Control.Applicative.Combinators (sepBy)
import Control.Applicative
import qualified Data.Map as Map

fileContent = parseContent $(getFile)

parseContent = unsafeParse parseGames

parseGame = do
  _ <- "Game "
  id :: Int <- parseNumber
  _ <- ": "
  draws <- parseDraw `sepBy` "; "

  pure (id, draws)

parseGames = some (parseGame <* "\n")

parseDraw = parseColor `sepBy` ", "

parseColor = do
  n :: Int <- parseNumber
  color <- "green" <|> "blue" <|> "red"
  pure (color, n)

-- * Generics


-- * FIRST problem
day ex = sum $ map fst $ filter snd $ zip [1..] $ map (checkGame . sumGame) ex


sumGame (_i, l) = Map.fromListWith f (concat l)
  where
    f = max

checkGame g = g Map.! "green" <= 13
          && g Map.! "red" <= 12
          && g Map.! "blue"<= 14

-- * SECOND problem
day' ex = sum $ fmap product $ Map.elems . sumGame <$> ex

-- * Tests

ex = parseContent [str|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]
