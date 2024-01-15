{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Day01 where

import Data.Char (isDigit)
import Data.Text qualified as T
import Utils

fileContent = parseContent $(getFile)

parseContent :: Text -> [Text]
parseContent = T.lines

-- * Generics

extractNumber = (\t -> read @Int [T.head t, T.last t]) . T.filter isDigit

convertionMapLeft =
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

convertionMapRight = map (\(key, val) -> (reverse key, val)) convertionMapLeft

convertNumbers = convertNumbersLeft . convertNumbersRight

convert t convertionMap = T.pack $ go (T.unpack t)
  where
    go [] = []
    go s = go' convertionMap s

    go' [] [] = []
    go' _ [] = []
    go' [] (_x:xs) = go xs
    
    go' ((prefix, repl):m') s@(x:_xs)
      | isDigit x = [x]
      | prefix `isPrefixOf` s = repl
      | otherwise = go' m' s

convertNumbersLeft t = convert t convertionMapLeft
convertNumbersRight t = convert (T.reverse t) convertionMapRight

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
