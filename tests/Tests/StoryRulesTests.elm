module Tests.StoryRulesTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)
import StoryRules exposing (..)


type TestStoryElements
    = ThingOne
    | ThingTwo
    | Jack
    | Jill
    | Earth
    | Moon


type TestScene
    = Begining


startingState : StoryState TestStoryElements TestScene
startingState =
    StoryState.init Earth Begining


all : Test
all =
    describe "StoryRulesTests"
        [ updateFromRulesTests
        , matchesTriggerTests
        , matchesConditionTests
        ]


updateFromRulesTests : Test.Test
updateFromRulesTests =
    describe "updateFromRules"
        [ test "with no matches returns nothing"
            <| \() ->
                let
                    rules =
                        [ given (InteractionWith ThingTwo) (Always)
                            `do` [ AddInventory ThingOne ]
                            `narrate` Simple "thing one"
                        ]
                in
                    Expect.equal (updateFromRules ThingOne (always rules) startingState (always True))
                        Nothing
        , test "only runs first matching rule"
            <| \() ->
                let
                    rules =
                        [ given (InteractionWith ThingTwo) (Always)
                            `do` [ AddInventory ThingOne ]
                            `narrate` Simple "no match"
                        , given (InteractionWith ThingOne) (Always)
                            `do` [ AddInventory ThingOne ]
                            `narrate` Simple "first match"
                        , given (InteractionWith ThingOne) (Always)
                            `do` [ AddInventory ThingTwo ]
                            `narrate` Simple "also matches"
                        ]

                    expected =
                        { startingState
                            | inventory = [ ThingOne ]
                            , storyLine = [ ( ThingOne, "first match" ) ]
                        }
                in
                    Expect.equal (updateFromRules ThingOne (always rules) startingState (always True))
                        (Just expected)
        ]


matchesTriggerTests : Test.Test
matchesTriggerTests =
    describe "matchesTrigger"
        [ describe "InteractionWith"
            [ test "a match"
                <| \() ->
                    Expect.true "match"
                        (matchesTrigger (InteractionWith ThingOne) ThingOne (always True))
            , test "no match"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (InteractionWith ThingOne) ThingTwo (always True))
            ]
        , describe "FirstInteractionWith"
            [ test "a match"
                <| \() ->
                    Expect.true "match"
                        (matchesTrigger (FirstInteractionWith ThingOne) ThingOne (always False))
            , test "no match (not first interaction)"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (FirstInteractionWith ThingOne) ThingOne (always True))
            , test "no match (different story element)"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (FirstInteractionWith ThingOne) ThingTwo (always False))
            ]
        ]


matchesConditionTests : Test.Test
matchesConditionTests =
    describe "matchesCondition"
        [ test "Always"
            <| \() ->
                Expect.true "always true"
                    <| matchesCondition Always startingState
        , describe "WithItem"
            [ test "match"
                <| \() ->
                    Expect.true "match"
                        <| matchesCondition (WithItem ThingOne)
                        <| addInventory ThingOne startingState
            , test "no match"
                <| \() ->
                    Expect.false "no match"
                        <| matchesCondition (WithItem ThingOne)
                        <| addInventory ThingTwo startingState
            ]
        , describe "NearCharacter"
            [ test "match"
                <| \() ->
                    addCharacter Jack Earth startingState
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    addCharacter Jill Earth startingState
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.false "no match"
            ]
        , describe "NearProp"
            [ test "match"
                <| \() ->
                    addProp ThingOne Earth startingState
                        |> matchesCondition (NearProp ThingOne)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    addProp ThingTwo Earth startingState
                        |> matchesCondition (NearProp ThingOne)
                        |> Expect.false "no match"
            ]
        , describe "InLocation"
            [ test "match"
                <| \() ->
                    startingState
                        |> matchesCondition (InLocation Earth)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    startingState
                        |> matchesCondition (InLocation Moon)
                        |> Expect.false "no match"
            ]
        , describe "All"
            [ test "match"
                <| \() ->
                    addProp ThingOne Earth startingState
                        |> matchesCondition
                            (All
                                [ (InLocation Earth)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    addProp ThingTwo Earth startingState
                        |> matchesCondition
                            (All
                                [ (InLocation Earth)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.false "no match"
            ]
        , describe "Any"
            [ test "match"
                <| \() ->
                    addProp ThingOne Earth startingState
                        |> matchesCondition
                            (Any
                                [ (InLocation Moon)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    addProp ThingOne Earth startingState
                        |> matchesCondition
                            (Any
                                [ (InLocation Moon)
                                , (NearProp ThingTwo)
                                ]
                            )
                        |> Expect.false "no match"
            ]
        , describe "Not"
            [ test "match"
                <| \() ->
                    startingState
                        |> matchesCondition (Not (InLocation Moon))
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    startingState
                        |> matchesCondition (Not (InLocation Earth))
                        |> Expect.false "no match"
            ]
        ]
