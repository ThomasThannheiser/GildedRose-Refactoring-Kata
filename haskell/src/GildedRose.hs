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
    updateQualityItem item@(Item name _ quality) =
      Item name (decSellIn item) (sellOutFkt item . updateQualityFkt item $ quality)

decQuality :: Int -> Int
decQuality quality = quality - if quality > 0 then 1 else 0

incQuality :: Int -> Int
incQuality quality = quality + if quality < 50 then 1 else 0

decSellIn :: Item -> Int
decSellIn (Item name sellIn _) = sellIn - if name == "Sulfuras, Hand of Ragnaros" then 0 else 1

sellOutFkt :: Item -> (Int -> Int)
sellOutFkt (Item name sellIn _)= 
  if sellIn < 1 then
    case name of
      "Aged Brie" -> incQuality
      "Backstage passes to a TAFKAL80ETC concert" -> const 0
      "Sulfuras, Hand of Ragnaros" -> id
      _ -> decQuality
  else id

updateQualityFkt :: Item -> (Int -> Int)
updateQualityFkt (Item name sellIn _) = case name of
  "Aged Brie" -> incQuality
  "Backstage passes to a TAFKAL80ETC concert" ->
    incQuality 
    . (if sellIn < 11 then incQuality else id)  
    . (if sellIn < 6 then incQuality else id)
  "Sulfuras, Hand of Ragnaros" -> id
  _ -> decQuality