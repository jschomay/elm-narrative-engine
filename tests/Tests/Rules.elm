module Tests.Rules exposing (all)

import Engine.Rules
import Test exposing (..)
import Expect
import Types exposing (..)
import Dict


all : Test
all =
    describe "Rules"
        [ describe "findMatchingRule"
            [ test "at the same specificity and without scene constraints, rules with more conditions win" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule2, ruleX ]
                    in
                        Expect.equal (Just rule2) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "at the same specificity and without scene constraints, rules with more conditions win (sanity check)" <|
                \() ->
                    let
                        rules =
                            [ rule2, rule1, ruleX ]
                    in
                        Expect.equal (Just rule2) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
              -- the following would be good fuzz test candidates...
            , test "any specific-matching rule always beats any non-specific-matching rule" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule4, ruleX ]
                    in
                        Expect.equal (Just rule1) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "any entity-class-matching rule always beats any anything-matching rule" <|
                \() ->
                    let
                        rules =
                            [ rule6, rule4, ruleX ]
                    in
                        Expect.equal (Just rule4) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "any scene-specific rule always beats any non-scene-specific rule" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule3, rule2, ruleX ]
                    in
                        Expect.equal (Just rule3) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "scene-specific rules trump specific-matching rules" <|
                \() ->
                    let
                        rules =
                            [ rule5, rule2, ruleX ]
                    in
                        Expect.equal (Just rule5) <|
                            Engine.Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            ]
        ]


story =
    { currentLocation = "x"
    , currentScene = "x"
    , manifest = Dict.fromList [ ( "x", Item False ItemInInventory ) ]
    , history = []
    , rules = Dict.empty
    , theEnd = Nothing
    }


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


ruleX =
    ( "7"
    , Rule (WithItem "does not exist")
        []
        []
    )
