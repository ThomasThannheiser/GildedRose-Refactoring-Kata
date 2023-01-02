module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem item@(Item name sellIn quality) =
      if name == "Sulfuras, Hand of Ragnaros" then 
        item
      else
        let sellOutFkt f = if sellIn < 1 then f else id
            updFkt f = sellOutFkt f . f
        in Item name (sellIn - 1)
            ((case name of
                "Aged Brie" ->
                  updFkt incQuality
                "Backstage passes to a TAFKAL80ETC concert" ->
                  sellOutFkt $ const 0 .
                  incQuality
                  . (if sellIn < 11 then incQuality else id)
                  . (if sellIn <  6 then incQuality else id)
                "Conjured Mana Cake" ->
                  updFkt $ decQuality . decQuality
                _ ->
                  updFkt decQuality)
            quality)

decQuality :: Int -> Int
decQuality quality = quality - if quality > 0 then 1 else 0

incQuality :: Int -> Int
incQuality quality = quality + if quality < 50 then 1 else 0
