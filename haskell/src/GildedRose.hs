module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show :: Item -> String
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem item@(Item name sellIn quality)
      | name == "Sulfuras, Hand of Ragnaros" = item
      | otherwise =
          let sellOutFkt f = if sellIn < 1 then f else id
              updFkt f = sellOutFkt f . f
          in Item name (sellIn - 1) $
                case name of
                  "Aged Brie" ->
                    updFkt incQuality
                  "Backstage passes to a TAFKAL80ETC concert" ->
                    sellOutFkt $ const 0
                    . incQuality
                    . (if sellIn < 11 then incQuality else id)
                    . (if sellIn <  6 then incQuality else id)
                  "Conjured Mana Cake" ->
                    updFkt $ decQuality . decQuality
                  _ ->
                    updFkt decQuality
                $ quality

decQuality :: Int -> Int
decQuality quality = quality - fromEnum (quality > 0)

incQuality :: Int -> Int
incQuality quality = quality + fromEnum (quality < 50)
