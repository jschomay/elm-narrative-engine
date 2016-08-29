module Tests.EngineTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Engine exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)
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


startingModel : Model TestItem TestLocation TestCharacter TestScene
startingModel =
    Engine.init "test" startingState


startingState : StoryState TestItem TestLocation TestCharacter TestScene
startingState =
    StoryState.init Earth Begining


all : Test
all =
    describe "EngineTests"
        [ updateTests ]


updateTests : Test.Test
updateTests =
    describe "update"
        [ describe "Interacttion"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        info a =
                            item "name" "description"

                        storyRules a =
                            [ given (InteractionWithItem ThingOne) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "custom" ) ] }
            , test "defaults to adding description to storyline"
                <| \() ->
                    let
                        info a =
                            item "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "description" ) ] }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        info a =
                            item "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Item ThingOne) startingModel).interactions
                            [ Item ThingOne ]
            ]
        , describe "InteractWithLocation"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        info a =
                            location "name" "description"

                        storyRules a =
                            [ given (InteractionWithLocation Earth) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Location Earth) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "custom" ) ] }
            , test "defaults to moving to location"
                <| \() ->
                    let
                        info a =
                            location "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Location Moon) startingModel).storyState
                            { startingState | currentLocation = Moon }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        info a =
                            location "name" "description"

                        storyRules a =
                            []
                    in
                        Expect.equal (update info info info storyRules (Interaction <| Location Earth) startingModel).interactions
                            [ Location Earth ]
            ]
        ]
