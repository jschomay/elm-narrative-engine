module Rules exposing (all)

import Dict
import Expect
import Narrative.Rules exposing (..)
import Narrative.Store exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Rule tests"
        [ test "findMatchingRule finds the right rule" <|
            \() ->
                let
                    rules =
                        [ rule1, rule2, ruleX ]
                in
                Expect.equal (Just rule2) <|
                    Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
        , describe "finding the test match"
            [ test "all else equal, rules with more conditions win" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule2, ruleX ]
                    in
                    Expect.equal (Just rule2) <|
                        Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "all else equal, rules with more conditions win (sanity check)" <|
                \() ->
                    let
                        rules =
                            [ rule2, rule1, ruleX ]
                    in
                    Expect.equal (Just rule2) <|
                        Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"

            -- the following would be good fuzz test candidates...
            , test "ID-matching trigger rules beat query-matching trigger rules (regardless of conditions)" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule4, ruleX ]

                        -- TODO test with ID-matching conditions too
                    in
                    Expect.equal (Just rule1) <|
                        Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
            , test "all else equal, ID-matching conditions beat query-matching conditions" <|
                \() ->
                    let
                        rules =
                            [ rule1, rule4, ruleX ]
                    in
                    Expect.equal (Just rule1) <|
                        Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
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
    , Rule (With "x")
        [ CurrentLocationIs "x" ]
        []
    )


rule2 =
    ( "2"
    , Rule
        (With "x")
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        ]
        []
    )


rule3 =
    ( "3"
    , Rule
        (With "x")
        [ CurrentSceneIs "x" ]
        []
    )


rule4 =
    ( "4"
    , Rule
        WithAnyItem
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        ]
        []
    )


rule5 =
    ( "5"
    , Rule WithAnyItem
        [ CurrentSceneIs "x" ]
        []
    )


rule6 =
    ( "6"
    , Rule
        WithAnything
        [ CurrentLocationIs "x"
        , ItemIsInInventory "x"
        , ItemIsInInventory "x"
        ]
        []
    )


ruleX =
    ( "7"
    , Rule (With "does not exist")
        []
        []
    )
