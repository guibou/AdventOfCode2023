{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day07Spec where

import Day07
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 6440
    it "of second star" $ do
      day' ex `shouldBe` 5905
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 246163188
    it "on second star" $ do
      day' fileContent `shouldBe` 245794069
