module Tests.RulesTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Story.State exposing (..)
import Story.Element exposing (..)
import Story.Rule exposing (..)


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
    Story.State.init Earth Begining


all : Test
all =
    describe "StoryRulesTests"
        [ updateFromRulesTests
        , matchesTriggerTests
        , matchesConditionTests
        ]


updateFromRulesTests : Test.Test
updateFromRulesTests =
    describe "findMatchingRule"
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
                    Expect.equal (findMatchingRule (Item ThingOne) rules startingState True)
                        Nothing
        , test "only runs first matching rule"
            <| \() ->
                let
                    rules =
                        [ interactingWith (Item ThingTwo)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingOne ]
                            `narrates` "first match"
                        , interactingWith (Item ThingOne)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingOne ]
                            `narrates` "second match"
                        , interactingWith (Item ThingOne)
                            `when` (everyTime)
                            `changesWorld` [ AddInventory ThingTwo ]
                            `narrates` "also matches"
                        ]

                    expected =
                        List.head rules
                in
                    Expect.equal (findMatchingRule (Item ThingTwo) rules startingState True)
                        (Maybe.map snd expected)
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
                        <| (advanceStory "ThingOne" startingState <| ( [ AddInventory ThingOne ], Narrate "AddInventory ThingOne" ))
            , test "no match"
                <| \() ->
                    Expect.false "no match"
                        <| matchesCondition (WithItem ThingOne)
                        <| (advanceStory "ThingTwo" startingState <| ( [ AddInventory ThingTwo ], Narrate "AddInventory ThingTwo" ))
            ]
        , describe "NearCharacter"
            [ test "match"
                <| \() ->
                    (advanceStory "Jack" startingState <| ( [ AddCharacter Jack Earth ], Narrate ".." ))
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    (advanceStory "Jill" startingState <| ( [ AddCharacter Jill Earth ], Narrate ".." ))
                        |> matchesCondition (NearCharacter Jack)
                        |> Expect.false "no match"
            ]
        , describe "NearProp"
            [ test "match"
                <| \() ->
                    (advanceStory "ThingOne" startingState <| ( [ AddProp ThingOne Earth ], Narrate ".." ))
                        |> matchesCondition (NearProp ThingOne)
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    (advanceStory "ThingTwo" startingState <| ( [ AddProp ThingTwo Earth ], Narrate ".." ))
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
                    (advanceStory "ThingOne" startingState <| ( [ AddProp ThingOne Earth ], Narrate ".." ))
                        |> matchesCondition
                            (All
                                [ (InLocation Earth)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    (advanceStory "ThingTwo" startingState <| ( [ AddProp ThingTwo Earth ], Narrate ".." ))
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
                    (advanceStory "ThingOne" startingState <| ( [ AddProp ThingOne Earth ], Narrate ".." ))
                        |> matchesCondition
                            (Any
                                [ (InLocation Moon)
                                , (NearProp ThingOne)
                                ]
                            )
                        |> Expect.true "match"
            , test "no match"
                <| \() ->
                    (advanceStory "ThingOne" startingState <| ( [ AddProp ThingOne Earth ], Narrate ".." ))
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
