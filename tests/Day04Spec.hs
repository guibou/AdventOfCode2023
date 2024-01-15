{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day04Spec where

import Test.Syd
import Day04

default (Int)

spec :: Spec
spec = do
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
