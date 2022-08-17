module GildedRose exposing (Item, updateItemQuality)


type alias Item =
    { name : String
    , sell_by : Int
    , quality : Int
    }


updateItemQuality : Item -> Item
updateItemQuality item =
    if item.name == "Sulfuras, Hand of Ragnaros" then
        item

    else if item.name == "Aged Brie" then
        updateAgedBrie item

    else if item.name == "Backstage passes to a TAFKAL80ETC concert" then
        updateBackstagePass item

    else
        updateGenericItem item


updateGenericItem : Item -> Item
updateGenericItem item =
    let
        quality =
            if item.sell_by < 0 then
                item.quality - 2

            else
                item.quality - 1
    in
    { item | sell_by = item.sell_by - 1, quality = max 0 quality }


updateBackstagePass : Item -> Item
updateBackstagePass item =
    let
        quality =
            if item.sell_by < 0 then
                0

            else if item.sell_by < 6 then
                item.quality + 3

            else if item.sell_by < 11 then
                item.quality + 2

            else
                item.quality + 1
    in
    { item | sell_by = item.sell_by - 1, quality = clamp 0 50 quality }


updateAgedBrie : Item -> Item
updateAgedBrie item =
    { item | sell_by = item.sell_by - 1, quality = clamp 0 50 <| item.quality + 1 }
