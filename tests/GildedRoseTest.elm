module GildedRoseTest exposing (initial)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import GildedRose exposing (Item, updateItemQuality)
import Test exposing (Test, describe, fuzz)


initial : Test
initial =
    describe "Initial behaviour"
        [ describe "Generic items"
            [ testGeneric
                "sell_in decreases every day"
                (\item updated -> Expect.lessThan item.sell_by updated.sell_by)
            , testGeneric
                "quality decreses every day if positive"
                (\item updated ->
                    if item.quality == 0 then
                        Expect.equal 0 updated.quality

                    else
                        Expect.lessThan item.quality updated.quality
                )
            , testGeneric
                "quality decreses twice as fast if the sell_by date has passed"
                (\item updated ->
                    Expect.equal
                        (case item.quality of
                            0 ->
                                0

                            1 ->
                                0

                            _ ->
                                if item.sell_by < 0 then
                                    item.quality - 2

                                else
                                    item.quality - 1
                        )
                        updated.quality
                )
            ]
        , fuzz itemFuzzer
            "The quality of an item is never negative"
            (Expect.all
                [ \item -> Expect.atLeast 0 item.quality
                , \item ->
                    Expect.atLeast 0 (updateItemQuality item).quality
                ]
            )
        , testSpecific "Aged Brie"
            "The quality of Aged Brie increases the older it gets (up to 50)"
            (\item updated ->
                if item.quality == 50 then
                    Expect.equal 50 updated.quality

                else
                    Expect.greaterThan item.quality updated.quality
            )
        , fuzz itemFuzzer
            "The quality of an item is never more than 50"
            (\item -> Expect.atMost 50 (updateItemQuality item).quality)
        , testSpecific "Sulfuras, Hand of Ragnaros"
            "Sulfuras' quality never decreases"
            (\item updated -> Expect.equal item.quality updated.quality)
        , testSpecific "Backstage passes to a TAFKAL80ETC concert"
            "Backstage passes get more quality as the day approaches, but are then worthless"
            (\item updated ->
                if item.quality >= 48 then
                    -- Backstage passes will not get more quality than 48
                    Expect.pass

                else if item.sell_by < 0 then
                    Expect.equal 0 updated.quality

                else if item.sell_by <= 5 then
                    Expect.equal (clamp 0 50 <| item.quality + 3) updated.quality

                else if item.sell_by <= 10 then
                    Expect.equal (clamp 0 50 <| item.quality + 2) updated.quality

                else
                    Expect.equal (clamp 0 50 <| item.quality + 1) updated.quality
            )
        , testSpecific "Conjured staff"
            "Conjured items degrade twice as fast"
            (\item updated ->
                Expect.equal
                    (case item.quality of
                        0 ->
                            0

                        1 ->
                            0

                        2 ->
                            0

                        3 ->
                            if item.sell_by < 0 then
                                0

                            else
                                1

                        _ ->
                            if item.sell_by < 0 then
                                item.quality - 4

                            else
                                item.quality - 2
                    )
                    updated.quality
            )
        ]


testSpecific : String -> String -> (Item -> Item -> Expectation) -> Test
testSpecific name description toExpectation =
    fuzz (specificItem name)
        description
        (\item ->
            toExpectation item (updateItemQuality item)
        )


testGeneric : String -> (Item -> Item -> Expectation) -> Test
testGeneric description toExpectation =
    fuzz genericItemFuzzer
        description
        (\item ->
            toExpectation item (updateItemQuality item)
        )


genericItemFuzzer : Fuzzer Item
genericItemFuzzer =
    specificItem "Foo"


specificItem : String -> Fuzzer Item
specificItem name =
    Fuzz.map3 Item
        (Fuzz.constant name)
        sellByFuzzer
        qualityFuzzer


itemFuzzer : Fuzzer Item
itemFuzzer =
    Fuzz.map3
        Item
        nameFuzzer
        sellByFuzzer
        qualityFuzzer


nameFuzzer : Fuzzer String
nameFuzzer =
    -- There are three names which are handled differently, then we add a generic string fuzzer for non-specific names
    Fuzz.oneOf
        [ Fuzz.constant "Aged Brie"
        , Fuzz.constant "Backstage passes to a TAFKAL80ETC concert"
        , Fuzz.constant "Sulfuras, Hand of Ragnaros"
        , Fuzz.string
        ]


sellByFuzzer : Fuzzer Int
sellByFuzzer =
    -- The critical points are 0, 6 and 11
    Fuzz.oneOf (List.map Fuzz.constant <| List.range -2 12)


qualityFuzzer : Fuzzer Int
qualityFuzzer =
    -- Critical points are 0, 1, 2, and 50
    -- We don't include -1 because the problem input says the quality is always nonnegative
    -- We don't include 51 because the problem input says the quality is never over 50. Except for "Sulfuras"
    Fuzz.oneOf (List.map Fuzz.constant <| List.range 0 50)
