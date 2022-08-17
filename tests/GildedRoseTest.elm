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
            ]
        , fuzz itemFuzzer
            "The quality of an item is never negative"
            (Expect.all
                [ \item -> Expect.atLeast 0 item.quality
                , \item ->
                    Expect.atLeast 0 (updateItemQuality item).quality
                ]
            )
        ]


testGeneric : String -> (Item -> Item -> Expectation) -> Test
testGeneric description toExpectation =
    fuzz genericItemFuzzer
        description
        (\item ->
            toExpectation item (updateItemQuality item)
        )


genericItemFuzzer : Fuzzer Item
genericItemFuzzer =
    Fuzz.map3 Item
        (Fuzz.constant "Foo")
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
    Fuzz.oneOf (List.map Fuzz.constant <| List.range -1 12)


qualityFuzzer : Fuzzer Int
qualityFuzzer =
    -- Critical points are 0, 1, 2, and 50
    -- We don't include -1 because the problem input says the quality is always nonnegative
    Fuzz.oneOf (List.map Fuzz.constant <| List.range 0 51)
