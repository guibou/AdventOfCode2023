module Day02Spec where

import Test.Syd
import Day02

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 8
    it "of second star" $ do
      day' ex `shouldBe` 2286
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2447
    it "on second star" $ do
      day' fileContent `shouldBe` 56322
-- started at Wed Dec 27 10:18:34 PM +04 2023
