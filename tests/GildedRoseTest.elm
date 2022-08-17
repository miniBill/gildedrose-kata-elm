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
            "the quality of an item is never negative"
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
    Fuzz.oneOf
        [ Fuzz.constant "Aged Brie"
        , Fuzz.constant "Backstage passes to a TAFKAL80ETC concert"
        , Fuzz.constant "Sulfuras, Hand of Ragnaros"
        , Fuzz.string
        ]


sellByFuzzer : Fuzzer Int
sellByFuzzer =
    Debug.todo "TODO"


qualityFuzzer : Fuzzer Int
qualityFuzzer =
    Debug.todo "TODO"
