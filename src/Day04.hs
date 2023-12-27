module Day04 where

import Utils
import Control.Applicative
import qualified Data.Set as Set
import Debug.Trace
import Data.Foldable
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (some (parseGame <* "\n"))


parseGame = do
  symbol "Card"
  n <- parseNumber
  symbol ":"
  wins <- some parseNumber
  symbol "|"
  mine <- some parseNumber

  pure (wins, mine)
-- * Generics


-- * FIRST problem
day :: _ -> Int
day = sum . fmap weightGame

weightGame x = let
  l = weightGame' x
  in if l > 0 then 2 ^ (l - 1) else 0

weightGame' (wins, mine) = let
  l = length $ (Set.intersection (Set.fromList mine) (Set.fromList wins))
  in l
-- * SECOND problem
day' :: _ -> Int
day' games = sum $ Map.elems $ foldl' (flip f) (Map.fromList (zip [1..] (games $> 1))) (zip [1..] games)
  where
    f (cardId, game) m = Map.unionsWith (+) [m,
                                                Map.fromList ((,m Map.! cardId) <$> (take (weightGame' game) [cardId + 1 .. ]))]

-- * Tests

ex = parseContent [str|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 13
    it "of second star" $ do
      day' ex `shouldBe` 30
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 28538
    it "on second star" $ do
      day' fileContent `shouldBe` 9425061
-- started at Wed Dec 27 11:22:07 PM +04 2023
