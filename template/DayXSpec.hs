{-# OPTIONS_GHC -Wno-type-defaults #-}
module DayXSpec where

import DayX
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` _
    it "of second star" $ do
      day' ex `shouldBe` _
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` _
    it "on second star" $ do
      day' fileContent `shouldBe` _
