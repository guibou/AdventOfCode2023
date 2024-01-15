{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day06Spec where

import Day06
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 288
    it "of second star" $ do
      day' ex `shouldBe` 71503
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2065338
    it "on second star" $ do
      day' fileContent `shouldBe` 34934171
