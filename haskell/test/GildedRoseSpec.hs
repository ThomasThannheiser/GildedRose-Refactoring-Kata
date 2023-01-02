module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "updateQuality" $ do

    it "fixme" $
       let inventory = [Item "+5 Dexterity Vest" 10  20]
           actual = updateQuality inventory
           expected = [Item "+5 Dexterity Vest" 9  19]
       in actual `shouldBe` expected

main :: IO ()
main = hspec $ do spec 