module Day08 (day, day', ex, ex2, ex', fileContent) where

import Utils
import qualified Data.Map.Strict as Map
import Control.Applicative
import Text.Megaparsec (oneOf)
import Data.Foldable (foldl')

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  directions <- some (("L" $> L) <|> ("R" $> R))
  _ <- "\n"
  _ <- "\n"
  nodes <- some (parseNode <* "\n")
  pure (directions, Map.fromList nodes)

parseNodeName = some (oneOf $ ['A'..'Z'] <> ['0'..'9'])

parseNode = do
  t <- parseNodeName
  _ <- " = ("
  l <- parseNodeName
  _ <- ", "
  r <- parseNodeName
  _ <- ")"

  pure (t, (l, r))

-- * Generics
data Direction = L | R
  deriving Show

-- * FIRST problem
day ex = findTermination "AAA" (=="ZZZ") ex

nextNode m i currentNode = f $ m Map.! currentNode
      where
        f = case i of
          L -> fst
          R -> snd

-- * SECOND problem
day' ex = foldl' lcm 1 (solve ex)

isFinalNode x = last x == 'Z'

findStartingNodes m = filter (\k -> last k == 'A') (Map.keys m)

findTermination start endC (instructions, m) = do
  let go (i:is) node !count
            | endC node = count
            | otherwise = go is (nextNode m i node) (count + 1)
      go [] node count = go instructions node count
  go instructions start 0

solve pb = map (\n -> findTermination n isFinalNode pb) (findStartingNodes (snd pb))

ex = parseContent [str|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|]

ex' = parseContent [str|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|]

ex2 = parseContent [str|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|]

-- started at Mon Jan 15 02:44:13 PM +04 2024
