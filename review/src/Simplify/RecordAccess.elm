module Simplify.RecordAccess exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Writer
import List.Extra
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ Simplify.RecordAccess.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules Simplify.RecordAccess
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Simplify.RecordAccess" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        Expression.RecordAccess record field ->
            let
                fieldName =
                    Node.value field
            in
            case Node.value record of
                Expression.RecordExpr setters ->
                    trySimplify setters fieldName node

                Expression.RecordUpdateExpression _ setters ->
                    trySimplify setters fieldName node

                _ ->
                    []

        _ ->
            []


trySimplify : List (Node RecordSetter) -> String -> Node Expression -> List (Rule.Error {})
trySimplify setters fieldName node =
    let
        setterValues =
            List.map
                (\setter ->
                    let
                        ( nfield, nvalue ) =
                            Node.value setter
                    in
                    ( Node.value nfield, nvalue )
                )
                setters
    in
    case
        List.Extra.find (\( setterField, _ ) -> setterField == fieldName) setterValues
    of
        Just ( _, setterValue ) ->
            [ Rule.errorWithFix
                { message = "Field access can be simplified"
                , details = [ "Accessing the field of a known record can be simplified to just that field's value" ]
                }
                (Node.range node)
                [ Fix.replaceRangeBy (Node.range node) (Elm.Writer.write <| Elm.Writer.writeExpression setterValue) ]
            ]

        Nothing ->
            []
