module DayXSpec where

import DayX
import Test.Syd

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
