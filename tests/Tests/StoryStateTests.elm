module Tests.StoryStateTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)


type TestLocation
    = Earth
    | Moon
    | Mars


type TestScene
    = Begining
    | Middle
    | End


all : Test
all =
    describe "StoryStateTests"
        [ describe "setCurrentLocation"
            [ test "sets the current location"
                <| \() ->
                    let
                        currentState =
                            StoryState.init Earth Begining
                    in
                        Expect.equal { currentState | currentLocation = Mars }
                            <| setCurrentLocation Mars currentState
            ]
        , describe "addLocation"
            [ test "prepend location to known locations"
                <| \() ->
                    let
                        init =
                            StoryState.init Earth Begining

                        currentState =
                            { init
                                | knownLocations = [ Earth ]
                            }
                    in
                        Expect.equal { currentState | knownLocations = [ Mars, Earth ] }
                            <| addLocation Mars currentState
            ]
        , describe "removeLocation"
            [ test "removes location from known locations"
                <| \() ->
                    let
                        init =
                            StoryState.init Earth Begining

                        currentState =
                            { init
                                | knownLocations = [ Earth, Moon, Mars ]
                            }
                    in
                        Expect.equal { currentState | knownLocations = [ Earth, Mars ] }
                            <| removeLocation Moon currentState
            , test "does nothing if location is not in known locations"
                <| \() ->
                    let
                        init =
                            StoryState.init Earth Begining

                        currentState =
                            { init
                                | knownLocations = [ Moon, Mars ]
                            }
                    in
                        Expect.equal currentState
                            <| removeLocation Earth currentState
            ]
        ]



-- etc
