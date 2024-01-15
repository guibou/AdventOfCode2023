module Day08 where

import Utils
import qualified Data.Map.Strict as Map
import Control.Applicative
import Text.Megaparsec (oneOf)

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  directions <- some (("L" $> L) <|> ("R" $> R))
  "\n"
  "\n"
  nodes <- some (parseNode <* "\n")
  pure (directions, Map.fromList nodes)

parseNodeName = some (oneOf ['A'..'Z'])

parseNode = do
  t <- parseNodeName
  " = ("
  l <- parseNodeName
  ", "
  r <- parseNodeName
  ")"

  pure (t, (l, r))

-- * Generics
data Direction = L | R
  deriving Show

-- * FIRST problem
day (instructions, m) = go (cycle instructions) startNode 0
  where
    startNode = "AAA"

    go _ "ZZZ" !count = count
    go (i:is) currentNode !count = go is (f $ m Map.! currentNode) (count + 1)
      where
        f = case i of
          L -> fst
          R -> snd

-- * SECOND problem
day' = undefined

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

-- started at Mon Jan 15 02:44:13 PM +04 2024
