{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day03Spec where

import Test.Syd
import Day03

default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 4361
    it "of second star" $ do
      day' ex `shouldBe` 467835
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 498559
    it "on second star" $ do
      day' fileContent `shouldBe` 72246648
-- started at Wed Dec 27 10:43:20 PM +04 2023
