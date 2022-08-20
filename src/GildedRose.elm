module GildedRose exposing (Item, updateItemQuality)


type alias Item =
    { name : String
    , sellIn : Int
    , quality : Int
    }


updateItemQuality : Item -> Item
updateItemQuality item =
    let
        withUpdatedQuality : Item
        withUpdatedQuality =
            if
                (item.name /= "Aged Brie")
                    && (item.name /= "Backstage passes to a TAFKAL80ETC concert")
            then
                if item.quality > 0 then
                    if item.name /= "Sulfuras, Hand of Ragnaros" then
                        { item | quality = item.quality - 1 }

                    else
                        item

                else
                    item

            else if item.quality < 50 then
                let
                    item2 : Item
                    item2 =
                        { item | quality = item.quality + 1 }
                in
                if item2.name == "Backstage passes to a TAFKAL80ETC concert" then
                    let
                        item3 : Item
                        item3 =
                            if item2.sellIn < 11 then
                                if item2.quality < 50 then
                                    { item2 | quality = item2.quality + 1 }

                                else
                                    item2

                            else
                                item2
                    in
                    if item3.sellIn < 6 then
                        if item3.quality < 50 then
                            { item3 | quality = item3.quality + 1 }

                        else
                            item3

                    else
                        item3

                else
                    item2

            else
                item

        withSulfurasCheck : Item
        withSulfurasCheck =
            if withUpdatedQuality.name /= "Sulfuras, Hand of Ragnaros" then
                { withUpdatedQuality | sellIn = withUpdatedQuality.sellIn - 1 }

            else
                withUpdatedQuality

        withSellInCheck : Item
        withSellInCheck =
            if withSulfurasCheck.sellIn < 0 then
                if withSulfurasCheck.name /= "Aged Brie" then
                    if withSulfurasCheck.name /= "Backstage passes to a TAFKAL80ETC concert" then
                        if withSulfurasCheck.quality > 0 then
                            if withSulfurasCheck.name /= "Sulfuras, Hand of Ragnaros" then
                                { withSulfurasCheck | quality = withSulfurasCheck.quality - 1 }

                            else
                                withSulfurasCheck

                        else
                            withSulfurasCheck

                    else
                        { withSulfurasCheck | quality = withSulfurasCheck.quality - withSulfurasCheck.quality }

                else if withSulfurasCheck.quality < 50 then
                    { withSulfurasCheck | quality = withSulfurasCheck.quality + 1 }

                else
                    withSulfurasCheck

            else
                withSulfurasCheck
    in
    withSellInCheck
