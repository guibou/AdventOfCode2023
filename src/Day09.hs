module Day09 where

import Utils
import Control.Applicative
import Debug.Trace

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  some (some parseNumber <* "\n")

-- * Generics
solveOasis s = do
  if all (==0) s
  then (0, 0)
  else do
    let
      next = zipWith subtract s (drop 1 s)
      (prevDiff, nextDiff) = solveOasis next
    (head s - prevDiff, last s + nextDiff)

-- * FIRST problem
day = sum . map (snd . solveOasis)

-- * SECOND problem
day' = sum . map (fst . solveOasis)

ex = parseContent [str|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
|]

-- started at Mon Jan 15 05:22:58 PM +04 2024
