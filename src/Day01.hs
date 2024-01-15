module Day01 where

import Data.Char (isDigit)
import Data.Text qualified as T
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Text]
parseContent t = T.lines t

-- * Generics

extractNumber = (\t -> read @Int [T.head t, T.last t]) . (T.filter isDigit)

convertionMap =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
  ]

convertNumbers = convertNumbersLeft . convertNumbersRight

convertNumbersLeft t = T.pack $ go (T.unpack t)
  where
    go [] = []
    go s = go' convertionMap s

    go' [] [] = []
    go' [] (x:xs) = go xs
    
    go' ((prefix, repl):m') s@(x:xs)
      | isDigit x = [x]
      | prefix `isPrefixOf` s = repl
      | otherwise = go' m' s

convertNumbersRight t = T.pack $ reverse $ go (reverse $ T.unpack t)
  where
    go [] = []
    go s = go' convertionMap s

    go' [] [] = []
    go' [] (x:xs) = go xs
    
    go' ((prefix, repl):m') s@(x:xs)
      | isDigit x = [x]
      | reverse prefix `isPrefixOf` s = repl
      | otherwise = go' m' s

-- * FIRST problem

day :: [Text] -> Int
day = sum . fmap extractNumber

-- * SECOND problem

day' :: [Text] -> Int
day' ts = sum $ fmap (\t -> read (T.unpack $ convertNumbersLeft t <> convertNumbersRight t)) ts

-- * Tests

ex :: [Text]
ex =
  parseContent
    [str|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|]

ex' =
  parseContent
    [str|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|]

-- 54970 is too low
-- started at Wed Dec 27 09:36:52 PM +04 2023
