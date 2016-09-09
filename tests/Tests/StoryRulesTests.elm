module Tests.StoryRulesTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)


type TestItem
    = ThingOne
    | ThingTwo


type TestLocation
    = Earth
    | Moon


type TestCharacter
    = Jack
    | Jill


type TestScene
    = Begining


type TestKnowledge
    = Secret


startingState : StoryState TestItem TestLocation TestCharacter TestScene TestKnowledge
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
                        [ interactingWith (Item ThingTwo)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingOne ]
                            `narrates` "thing one"
                        ]
                in
                    Expect.equal (updateFromRules (Item ThingOne) rules startingState True "name")
                        Nothing
        , test "only runs first matching rule"
            <| \() ->
                let
                    rules =
                        [ interactingWith (Item ThingTwo)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingOne ]
                            `narrates` "no match"
                        , interactingWith (Item ThingOne)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingOne ]
                            `narrates` "first match"
                        , interactingWith (Item ThingOne)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingTwo ]
                            `narrates` "also matches"
                        ]

                    expected =
                        { startingState
                            | inventory = [ ThingOne ]
                            , storyLine = [ ( "name", "first match" ) ]
                        }
                in
                    Expect.equal (updateFromRules (Item ThingOne) rules startingState True "name")
                        (Just expected)
        ]


matchesTriggerTests : Test.Test
matchesTriggerTests =
    describe "matchesTrigger"
        [ describe "InteractionWith"
            [ test "a match"
                <| \() ->
                    Expect.true "match"
                        (matchesTrigger (InteractionWith <| Item ThingOne) (Item ThingOne) True)
            , test "no match"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (InteractionWith <| Item ThingOne) (Item ThingTwo) True)
            ]
        , describe "FirstInteractionWith"
            [ test "a match"
                <| \() ->
                    Expect.true "match"
                        (matchesTrigger (FirstInteractionWith <| Item ThingOne) (Item ThingOne) False)
            , test "no match (not first interaction)"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (FirstInteractionWith <| Item ThingOne) (Item ThingOne) True)
            , test "no match (different story element)"
                <| \() ->
                    Expect.false "no match"
                        (matchesTrigger (FirstInteractionWith <| Item ThingOne) (Item ThingTwo) False)
            ]
        ]


matchesConditionTests : Test.Test
matchesConditionTests =
    describe "matchesCondition"
        [ test "EveryTime"
            <| \() ->
                Expect.true "always true"
                    <| matchesCondition EveryTime startingState
        , describe "WithItem"
            [ test "match"
                <| \() ->
                    Expect.true "match"
                        <| matchesCondition (WithItem ThingOne)
                        <| StoryState.addInventory ThingOne startingState
            , test "no match"
                <| \() ->
                    Expect.false "no match"
                        <| matchesCondition (WithItem ThingOne)
                        <| StoryState.addInventory ThingTwo startingState
            ]
        , describe "NearCharacter"
            [ test "match"
                <| \() ->
                    StoryState.addCharacter Jack Earth startingState
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    StoryState.addCharacter Jill Earth startingState
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.false "no match"
            ]
        , describe "NearProp"
            [ test "match"
                <| \() ->
                    StoryState.addProp ThingOne Earth startingState
                        |> matchesCondition (NearProp ThingOne)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    StoryState.addProp ThingTwo Earth startingState
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
                    StoryState.addProp ThingOne Earth startingState
                        |> matchesCondition
                            (All
                                [ (InLocation Earth)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    StoryState.addProp ThingTwo Earth startingState
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
                    StoryState.addProp ThingOne Earth startingState
                        |> matchesCondition
                            (Any
                                [ (InLocation Moon)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    StoryState.addProp ThingOne Earth startingState
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
                        |> matchesCondition (unless (InLocation Moon))
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    startingState
                        |> matchesCondition (unless (InLocation Earth))
                        |> Expect.false "no match"
            ]
        ]
