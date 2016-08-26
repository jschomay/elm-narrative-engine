module Tests.EngineTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Engine exposing (..)
import StoryElements exposing (..)
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


startingModel : Model TestStoryElements TestScene
startingModel =
    Engine.init "test" startingState


startingState : StoryState TestStoryElements TestScene
startingState =
    StoryState.init Earth Begining


all : Test
all =
    describe "EngineTests"
        [ updateTests ]


updateTests : Test.Test
updateTests =
    describe "update"
        [ describe "Interact"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            [ given (InteractionWith ThingOne) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update storyElements storyRules (Interact ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( ThingOne, "custom" ) ] }
            , test "defaults to adding description to storyline"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update storyElements storyRules (Interact ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( ThingOne, "description" ) ] }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update storyElements storyRules (Interact ThingOne) startingModel).interactions
                            [ ThingOne ]
            ]
        , describe "InteractWithLocation"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            [ given (InteractionWith ThingOne) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update storyElements storyRules (Interact ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( ThingOne, "custom" ) ] }
            , test "defaults to moving to location"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update storyElements storyRules (InteractWithLocation Moon) startingModel).storyState
                            { startingState | currentLocation = Moon }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyElements a =
                            DisplayInformation "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update storyElements storyRules (Interact ThingOne) startingModel).interactions
                            [ ThingOne ]
            ]
        ]
