module GildedRoseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (..)
import Test exposing (..)


initial : Test
initial =
    describe "Initial behaviour"
        [ test "example test"
            (\_ ->
                let
                    foo =
                        Item "foo" 10 30
                in
                Expect.equal foo.name "foo"
            )
        ]
