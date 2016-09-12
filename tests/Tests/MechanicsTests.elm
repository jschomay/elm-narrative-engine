module Tests.MechanicsTests exposing (all)

import Color exposing (blue)
import Test exposing (..)
import Expect exposing (..)
import Mechanics exposing (..)
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


startingState : StoryState TestItem TestLocation TestCharacter TestScene TestKnowledge
startingState =
    StoryState.init Earth Begining


all : Test
all =
    describe "MechanicsTests"
        [ updateTests
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
                        Expect.equal (update displayInfo storyRules (Interact <| Item ThingOne) startingState).storyLine
                            <| ( "name", "custom" )
                            :: startingState.storyLine
            , test "defaults to adding description to storyline"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interact <| Item ThingOne) startingState).storyLine
                            <| ( "name", "description" )
                            :: startingState.storyLine
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interact <| Item ThingOne) startingState).familiarWith
                            (Item ThingOne :: startingState.familiarWith)
            ]
        , describe "Interact with location"
            [ test "tries to update from rules first"
                <| \() ->
                    let
                        storyRules a =
                            [ interactingWith (location Earth) `when` (everyTime) `changesWorld` [] `narrates` "custom" ]
                    in
                        Expect.equal (update displayInfo storyRules (Interact <| Location Earth) startingState)
                            { startingState | storyLine = ( "name", "custom" ) :: startingState.storyLine }
            , test "defaults to moving to location and adding narration"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interact <| Location Moon) startingState)
                            { startingState
                                | currentLocation = Moon
                                , familiarWith = [ Location Moon, Location Earth ]
                                , storyLine = ( "name", "description" ) :: startingState.storyLine
                            }
            , test "always adds new story elements to the interaction list"
                <| \() ->
                    let
                        storyRules a =
                            []
                    in
                        Expect.equal (update displayInfo storyRules (Interact <| Location Earth) startingState).familiarWith
                            [ Location Earth ]
            ]
        ]


initTests : Test.Test
initTests =
    describe "init"
        [ test "adds starting locatino to inst of familiarWith"
            <| \() ->
                Expect.equal (init Earth Begining |> .familiarWith)
                    [ Location Earth ]
        ]
