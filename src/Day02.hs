module Day02 where

import Utils
import Control.Applicative.Combinators (sepBy)
import Control.Applicative
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse parseGames

parseGame = do
  "Game "
  id :: Int <- parseNumber
  ": "
  draws <- parseDraw `sepBy` "; "

  pure (id, draws)

parseGames = some (parseGame <* "\n")

parseDraw = do
  colors <- parseColor `sepBy` ", "
  pure colors

parseColor = do
  n :: Int <- parseNumber
  color <- "green" <|> "blue" <|> "red"
  pure (color, n)

-- * Generics


-- * FIRST problem
day :: _ -> Int
day ex = sum $ map fst $ filter (\(_, v) -> v) $ zip [1..] $ map checkGame $ map sumGame ex


sumGame (i, l) = Map.fromListWith f (concat l)
  where
    f = max

checkGame g = g Map.! "green" <= 13
          && g Map.! "red" <= 12
          && g Map.! "blue"<= 14

-- * SECOND problem
day' :: _ -> Int
day' ex = sum $ fmap product $ map Map.elems $ map sumGame ex

-- * Tests

ex = parseContent [str|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 8
    it "of second star" $ do
      day' ex `shouldBe` 2286
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2447
    it "on second star" $ do
      day' fileContent `shouldBe` 56322
-- started at Wed Dec 27 10:18:34 PM +04 2023
