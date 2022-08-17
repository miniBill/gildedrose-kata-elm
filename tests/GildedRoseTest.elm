module GildedRoseTest exposing (initial)

import Expect
import Fuzz exposing (Fuzzer)
import GildedRose exposing (Item, updateItemQuality)
import Test exposing (Test, describe, fuzz, test)


initial : Test
initial =
    describe "Initial behaviour"
        [ test "example test"
            (\_ ->
                let
                    foo : Item
                    foo =
                        Item "foo" 10 30
                in
                Expect.equal foo.name "foo"
            )
        , fuzz itemFuzzer
            "The quality of an item is never negative"
            (Expect.all
                [ \item -> Expect.atLeast 0 item.quality
                , \item ->
                    Expect.atLeast 0 (updateItemQuality item).quality
                ]
            )
        ]


itemFuzzer : Fuzzer Item
itemFuzzer =
    Fuzz.map3 Item nameFuzzer sellByFuzzer qualityFuzzer


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
