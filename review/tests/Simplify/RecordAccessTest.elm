module Simplify.RecordAccessTest exposing (all)

import Review.Test
import Simplify.RecordAccess exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Simplify.RecordAccess"
        [ test "should simplify record accesses for explicit records" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details =
                                [ "Accessing the field of a known record can be simplified to just that field's value"
                                ]
                            , under = "{ b = 3 }.b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "shouldn't simplify record accesses for explicit records if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for record updates" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 }.b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details =
                                [ "Accessing the field of a known record can be simplified to just that field's value"
                                ]
                            , under = "{ d | b = 3 }.b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "shouldn't simplify record accesses for record updates if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 }.c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
