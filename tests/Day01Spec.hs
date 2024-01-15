module Day01Spec where

import Test.Syd
import Day01

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 142
    it "of second star" $ do
      day' ex' `shouldBe` 281
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 55816
    it "on second star" $ do
      day' fileContent `shouldBe` 54980
