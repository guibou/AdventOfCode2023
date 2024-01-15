module Day07 where

import Utils
import Control.Applicative
import Text.Megaparsec (oneOf)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Maybe
import Debug.Trace

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ some parseHandBid

parseHandBid = do
  hand <- parseHand
  " "
  bid <- parseNumber
  "\n"

  pure (hand, bid)

parseHand = some (oneOf ("AKQJT9876543210" :: [Char]))


-- * Generics
rankHand hand = do
  let h = Map.fromListWith (+) $ do
        c <- hand
        pure (c, 1)
  (getRank h, map rankLetters hand)

getRank h = - case sort (Map.elems h) of
        [5] -> 0
        [1, 4] -> 1
        [2, 3] -> 2
        [1, 1, 3] -> 3
        [1, 2, 2] -> 4
        [1, 1, 1, 2] -> 5
        [1, 1, 1, 1, 1] -> 6
        e -> error $ "getRank: " <> show e

rankLetters '1' = 1
rankLetters '2' = 2
rankLetters '3' = 3
rankLetters '4' = 4
rankLetters '5' = 5
rankLetters '6' = 6
rankLetters '7' = 7
rankLetters '8' = 8
rankLetters '9' = 9
rankLetters 'T' = 10
rankLetters 'J' = 11
rankLetters 'Q' = 12
rankLetters 'K' = 13
rankLetters 'A' = 14


-- * FIRST problem
day handsAndBids = sum $ zipWith (*) [1..] (map snd $ sortOn (\(hand, _bid) -> rankHand hand) handsAndBids)

-- * SECOND problem
rankHand' hand = do
  let (traceShowId -> m') = Map.fromListWith (+) $ do
        c <- hand
        pure (c, 1)
  let j = fromMaybe 0 $ Map.lookup 'J' m'
      m = Map.delete 'J' m'
  let rank
        | j == 5 = 0
        | j == 0 = getRank m
        | otherwise = maximum $ do
         (traceShowId -> m') <- newHandFrom j (Map.keys m)
         pure $ getRank (Map.unionWith (+) m (Map.fromListWith (+) m'))
          
  (rank, map rankLetters' hand)

newHandFrom 0 keys = [[]]
newHandFrom n keys = do
  k <- keys
  l' <- ((k, 1):) <$> newHandFrom (n-1) keys
  pure $ l'

rankLetters' '1' = 1
rankLetters' '2' = 2
rankLetters' '3' = 3
rankLetters' '4' = 4
rankLetters' '5' = 5
rankLetters' '6' = 6
rankLetters' '7' = 7
rankLetters' '8' = 8
rankLetters' '9' = 9
rankLetters' 'T' = 10
rankLetters' 'J' = 0
rankLetters' 'Q' = 12
rankLetters' 'K' = 13
rankLetters' 'A' = 14


-- * FIRST problem
day' handsAndBids = sum $ zipWith (*) [1..] (map snd $ sortOn (\(hand, _bid) -> rankHand' hand) handsAndBids)



ex = parseContent [str|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]

-- started at Mon Jan 15 01:33:49 PM +04 2024
--
-- too high: 254837312
