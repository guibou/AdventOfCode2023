module Day07 where

import Utils
import Control.Applicative
import Text.Megaparsec (oneOf)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

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
  let h = sort $ Map.elems $ Map.fromListWith (+) $ do
        c <- hand
        pure (c, 1)

  let rank = case h of
        [5] -> 0
        [_, 4] -> 1
        [2, 3] -> 2
        [1, 1, 3] -> 3
        [1, 2, 2] -> 4
        [1, 1, 1, 2] -> 5
        [1, 1, 1, 1, 1] -> 6
  (-rank, map rankLetters hand)

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
day' = undefined

ex = parseContent [str|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]

-- started at Mon Jan 15 01:33:49 PM +04 2024
--
-- too high: 254837312
