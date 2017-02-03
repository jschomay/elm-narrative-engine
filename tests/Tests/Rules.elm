module Tests.Rules exposing (all)

import Engine.Rules
import Test exposing (..)
import Expect
import Types exposing (..)


all : Test
all =
    describe "Rules"
        [ describe "bestMatch"
            [ test "at the same specificity and without scene constraints, rules with more conditions win" <|
                \() ->
                    let
                        matchingRules =
                            [ rule1, rule2 ]
                    in
                        Expect.equal (Just rule2) <|
                            Engine.Rules.bestMatch matchingRules
            , test "at the same specificity and without scene constraints, rules with more conditions win (sanity check)" <|
                \() ->
                    let
                        matchingRules =
                            [ rule2, rule1 ]
                    in
                        Expect.equal (Just rule2) <|
                            Engine.Rules.bestMatch matchingRules
              -- the following would be good fuzz test candidates...
            , test "any specific-matching rule always beats any non-specific-matching rule" <|
                \() ->
                    let
                        matchingRules =
                            [ rule1, rule4 ]
                    in
                        Expect.equal (Just rule1) <|
                            Engine.Rules.bestMatch matchingRules
            , test "any entity-class-matching rule always beats any anything-matching rule" <|
                \() ->
                    let
                        matchingRules =
                            [ rule6, rule4 ]
                    in
                        Expect.equal (Just rule4) <|
                            Engine.Rules.bestMatch matchingRules
            , test "any scene-specific rule always beats any non-scene-specific rule" <|
                \() ->
                    let
                        matchingRules =
                            [ rule1, rule3, rule2 ]
                    in
                        Expect.equal (Just rule3) <|
                            Engine.Rules.bestMatch matchingRules
            , test "scene-specific rules trump specific-matching rules" <|
                \() ->
                    let
                        matchingRules =
                            [ rule5, rule2 ]
                    in
                        Expect.equal (Just rule5) <|
                            Engine.Rules.bestMatch matchingRules
            ]
        ]


rule1 =
    ( "1"
    , Rule (WithItem "x")
        [ CurrentLocationIs "x" ]
        []
    )


rule2 =
    ( "2"
    , Rule
        (WithItem "x")
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        ]
        []
    )


rule3 =
    ( "3"
    , Rule
        (WithItem "x")
        [ CurrentSceneIs "x" ]
        []
    )


rule4 =
    ( "4"
    , Rule
        (WithAnyItem)
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        ]
        []
    )


rule5 =
    ( "5"
    , Rule (WithAnyItem)
        [ CurrentSceneIs "x" ]
        []
    )


rule6 =
    ( "6"
    , Rule
        (WithAnything)
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        , ItemIsInInventory "x"
        ]
        []
    )
