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


type TestKnowledge
    = Secret


startingModel : Model TestItem TestLocation TestCharacter TestScene TestKnowledge
startingModel =
    Engine.init storyInfo storySetup


storyInfo : StoryInfo
storyInfo =
    { title = "title"
    , byline = "byline"
    , prologue = "prologue"
    }


startingState : StoryState TestItem TestLocation TestCharacter TestScene TestKnowledge
startingState =
    startingModel.storyState


storySetup : StorySetup TestItem TestLocation TestCharacter TestScene TestKnowledge
storySetup =
    { startingScene = Begining
    , startingLocation = Earth
    , startingNarration = "In the beginning..."
    , storyWorldSetupCommands = []
    }


all : Test
all =
    describe "EngineTests"
        [ updateTests
        , setUpStoryWorldTests
        , initTests
        ]


displayInfo : DisplayInfo TestItem TestLocation TestCharacter
displayInfo =
    { items = \_ -> itemInfo "name" "description"
    , locations = \_ -> locationInfo "name" blue "description"
    , characters = \_ -> characterInfo "name" "description"
    }


updateTests : Test.Test
updateTests =
    describe "update"
        [ describe "Interaction with Item"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyRules a =
                            [ interactingWith (item ThingOne) `when` (everyTime) `changesWorld` [] `narrates` "custom" ]
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = ( "name", "custom" ) :: startingState.storyLine }
            , test "defaults to adding description to storyline"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Item ThingOne) startingModel).storyState
                            { startingState | storyLine = ( "name", "description" ) :: startingState.storyLine }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Item ThingOne) startingModel).interactions
                            (Item ThingOne :: startingModel.interactions)
            ]
        , describe "Interact with location"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyRules a =
                            [ interactingWith (location Earth) `when` (everyTime) `changesWorld` [] `narrates` "custom" ]
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Location Earth) startingModel).storyState
                            { startingState | storyLine = ( "name", "custom" ) :: startingState.storyLine }
            , test "defaults to moving to location and adding narration"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Location Moon) startingModel).storyState
                            { startingState
                                | currentLocation = Moon
                                , storyLine = ( "name", "description" ) :: startingState.storyLine
                            }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interaction <| Location Earth) startingModel).interactions
                            [ Location Earth ]
            ]
        ]


initTests : Test.Test
initTests =
    describe "init"
        [ test "adds starting locatino to inst of interactions"
            <| \() ->
                Expect.equal (Engine.init storyInfo storySetup |> .interactions)
                    [ Location Earth ]
        ]


setUpStoryWorldTests : Test.Test
setUpStoryWorldTests =
    describe "setUpStoryWorld"
        [ test "sets up story state correctly"
            <| \() ->
                let
                    expectedStoryState =
                        { startingState | inventory = [ ThingOne ] }
                in
                    Expect.equal (setUpStoryWorld { storySetup | storyWorldSetupCommands = [ AddInventory ThingOne ] })
                        expectedStoryState
        ]
