module Tests.EngineTests exposing (all)

import Color exposing (blue)
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
    Engine.init "title" "byline" "prologue" startingState


startingState : StoryState TestItem TestLocation TestCharacter TestScene
startingState =
    StoryState.init Earth Begining


all : Test
all =
    describe "EngineTests"
        [ updateTests ]


itemInfo : a -> BasicInfo
itemInfo a =
    item "name" "description"


locationInfo : a -> WithColor BasicInfo
locationInfo a =
    location "name" blue "description"


characterInfo : a -> BasicInfo
characterInfo a =
    character "name" "description"


updateTests : Test.Test
updateTests =
    describe "update"
        [ describe "Interacttion"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyRules a =
                            [ given (InteractionWithItem ThingOne) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "custom" ) ] }
            , test "defaults to adding description to storyline"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "description" ) ] }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Item ThingOne) startingModel).interactions
                            [ Item ThingOne ]
            ]
        , describe "InteractWithLocation"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyRules a =
                            [ given (InteractionWithLocation Earth) (Always) `do` [] `narrate` Simple "custom" ]
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Location Earth) startingModel).storyState
                            { startingState | storyLine = [ ( "name", "custom" ) ] }
            , test "defaults to moving to location and adding narration"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Location Moon) startingModel).storyState
                            { startingState
                                | currentLocation = Moon
                                , storyLine = [ ( "name", "description" ) ]
                            }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update itemInfo locationInfo characterInfo storyRules (Interaction <| Location Earth) startingModel).interactions
                            [ Location Earth ]
            ]
        ]
