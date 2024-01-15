module Day03 where

import Utils
import Control.Monad
import qualified Data.Text as Text
import Data.Char
import qualified Data.Map as Map

fileContent :: Text
fileContent = $(getFile)

extractSymbol :: Text -> _
extractSymbol t = do
  (lineNo, line) <- zip [0..] (Text.lines t)
  (colNo, c) <- zip [0..] (Text.unpack line)
  guard $ c /= '.' && not (isDigit c)
  pure ((lineNo, colNo), c)

-- * Generics
sumNumbers t = do
  (lineNo, line) <- zip [0 :: Int ..] (Text.lines t)
  (colOffset, number) <- findNumbers line
  (line, col) <- squareAround lineNo colOffset (Text.length number)
  pure ((line, col), read @Int $ Text.unpack number)

findNumbers line = go 0 line
  where
   go offset t
     | Text.null t = []
     | otherwise = do
       let (prefix, suffix) = Text.span (not . isDigit) t
       let (number, rest) = Text.span isDigit suffix
       let lenPrefix = Text.length prefix
       (offset + lenPrefix, number) : go (offset + lenPrefix + Text.length number) rest

squareAround line colOffset len = [(line, colOffset -1), (line, colOffset + len)] <> do
  line' <- [line-1, line+1]
  col <- [colOffset-1..colOffset + len]
  pure (line', col)

sumMap t = do
  let
    symbols = Map.fromList $ extractSymbol t
    numsInfos = Map.fromListWith (\(a, b) (a', b') -> (a ++ a', b + b')) $ map (\(c, v) -> (c, ([v], 1))) $ sumNumbers t
  numsInfos
      


-- * FIRST problem
day t = do
  let
   symbols = Map.fromList $ extractSymbol t
  sum . concat . map fst . Map.elems . flip Map.intersection symbols . sumMap $ t

-- * SECOND problem
day' t = do
  let
   symbols = Map.fromList $ extractSymbol t
  sum . map product . map fst . filter (\(a, c) -> c == 2) . Map.elems . flip Map.intersection (Map.filter (== '*') symbols) . sumMap $ t

-- * Tests

ex :: Text
ex = [str|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|]
