module Tests.StoryStateTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)


type TestItem
    = ThingOne
    | ThingTwo


type TestLocation
    = Earth
    | Moon
    | Mars


type TestCharacter
    = Jack
    | Jill


type TestScene
    = Begining
    | Middle
    | End


init : StoryState TestItem TestLocation TestCharacter TestScene
init =
    StoryState.init Earth Begining


all : Test
all =
    describe "StoryStateTests"
        [ describe "setCurrentLocation"
            [ test "sets the current location"
                <| \() ->
                    Expect.equal { init | currentLocation = Mars }
                        <| setCurrentLocation Mars init
            ]
        , describe "addLocation"
            [ test "prepend location to known locations"
                <| \() ->
                    addLocation Earth init
                        |> addLocation Mars
                        |> .knownLocations
                        |> Expect.equal [ Mars, Earth ]
            , test "won't add a location twice"
                <| \() ->
                    addLocation Moon init
                        |> addLocation Mars
                        |> addLocation Mars
                        |> .knownLocations
                        |> Expect.equal [ Mars, Moon ]
            ]
        , describe "removeLocation"
            [ test "removes location from known locations without chaning order"
                <| \() ->
                    addLocation Moon init
                        |> addLocation Earth
                        |> addLocation Mars
                        |> removeLocation Earth
                        |> .knownLocations
                        |> Expect.equal [ Mars, Moon ]
            , test "does nothing if location is not in known locations"
                <| \() ->
                    addLocation Moon init
                        |> addLocation Mars
                        |> removeLocation Earth
                        |> .knownLocations
                        |> Expect.equal [ Mars, Moon ]
            ]
        , describe "addProp"
            [ test "appends prop to items by location "
                <| \() ->
                    addProp ThingOne Earth init
                        |> addProp ThingTwo Earth
                        |> getPropsInCurrentLocation
                        |> Expect.equal [ ThingOne, ThingTwo ]
            , test "wont duplicate items"
                <| \() ->
                    addProp ThingOne Earth init
                        |> addProp ThingTwo Earth
                        |> addProp ThingTwo Earth
                        |> getPropsInCurrentLocation
                        |> Expect.equal [ ThingOne, ThingTwo ]
            ]
        , describe "addCharacter"
            [ test "appends character to items by location "
                <| \() ->
                    addCharacter Jack Earth init
                        |> addCharacter Jill Earth
                        |> getCharactersInCurrentLocation
                        |> Expect.equal [ Jack, Jill ]
            , test "wont duplicate characters"
                <| \() ->
                    addCharacter Jack Earth init
                        |> addCharacter Jill Earth
                        |> addCharacter Jill Earth
                        |> getCharactersInCurrentLocation
                        |> Expect.equal [ Jack, Jill ]
            ]
        , describe "removeProp"
            [ test "removes the prop from items by location"
                <| \() ->
                    addProp ThingOne Earth init
                        |> addProp ThingTwo Earth
                        |> removeProp ThingOne Earth
                        |> getPropsInCurrentLocation
                        |> Expect.equal [ ThingTwo ]
            , test "does nothing if the location does not have the prop"
                <| \() ->
                    addProp ThingOne Earth init
                        |> removeProp ThingTwo Earth
                        |> getPropsInCurrentLocation
                        |> Expect.equal [ ThingOne ]
            , test "does nothing if the location is empty"
                <| \() ->
                    init
                        |> removeProp ThingOne Earth
                        |> getPropsInCurrentLocation
                        |> Expect.equal []
            , test "leaves other locations alone"
                <| \() ->
                    addProp ThingOne Earth init
                        |> addProp ThingTwo Earth
                        |> addProp ThingOne Mars
                        |> removeProp ThingOne Mars
                        |> getPropsInCurrentLocation
                        |> Expect.equal [ ThingOne, ThingTwo ]
            ]
        , describe "removeCharacter"
            [ test "removes the character from characters by location"
                <| \() ->
                    addCharacter Jill Earth init
                        |> addCharacter Jack Earth
                        |> removeCharacter Jill Earth
                        |> getCharactersInCurrentLocation
                        |> Expect.equal [ Jack ]
            , test "does nothing if the location does not have the character"
                <| \() ->
                    addCharacter Jill Earth init
                        |> removeCharacter Jack Earth
                        |> getCharactersInCurrentLocation
                        |> Expect.equal [ Jill ]
            , test "does nothing if the location is empty"
                <| \() ->
                    init
                        |> removeCharacter Jill Earth
                        |> getCharactersInCurrentLocation
                        |> Expect.equal []
            , test "leaves other locations alone"
                <| \() ->
                    addCharacter Jill Earth init
                        |> addCharacter Jack Earth
                        |> addCharacter Jill Mars
                        |> removeCharacter Jill Mars
                        |> getCharactersInCurrentLocation
                        |> Expect.equal [ Jill, Jack ]
            ]
        ]
