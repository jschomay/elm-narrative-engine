module Tests.StoryStateTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)
import StoryElements exposing (..)


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


type TestKnowledge
    = Secret


init : StoryState TestItem TestLocation TestCharacter TestScene TestKnowledge
init =
    StoryState.init Earth Begining


all : Test
all =
    describe "StoryStateTests"
        [ describe "advanceStory"
            [ describe "MoveTo"
                [ test "sets the current location"
                    <| \() ->
                        Expect.equal
                            { init
                                | currentLocation = Mars
                                , storyLine = [ ( "Mars", "MoveTo Mars" ) ]
                                , familiarWith = [ Location Earth, Location Mars ]
                            }
                            <| advanceStory "Mars" init
                            <| ( [ moveTo Mars ], Narrate "MoveTo Mars" )
                , describe "adds the location to the 'familiarWith' list (even if it isn't the subject of the interaction)"
                    [ test "if it isn't already there"
                        <| \() ->
                            Expect.equal [ Location Earth, Location Mars ]
                                <| .familiarWith
                                <| advanceStory "Jack" init
                                <| ( [ moveTo Mars ], Narrate "Jack sends you to Mars" )
                    , test "unless it is already there"
                        <| \() ->
                            Expect.equal [ Location Earth, Location Mars ]
                                <| .familiarWith
                                <| (\newState -> advanceStory "Jack" newState <| ( [ moveTo Mars ], Narrate "Jack sends you to Mars" ))
                                <| advanceStory "Mars" init
                                <| ( [ MoveTo Mars ], Narrate "I think I'll go to mars myself" )
                    ]
                ]
            , describe "AddLocation"
                [ test "prepend location to known locations"
                    <| \() ->
                        Expect.equal { init | knownLocations = [ Mars, Earth ], storyLine = [ ( "Mars", "AddLocation Mars" ), ( "Earth", "AddLocation Earth" ) ] }
                            <| (\newState -> advanceStory "Mars" newState <| ( [ AddLocation Mars ], Narrate "AddLocation Mars" ))
                            <| advanceStory "Earth" init
                            <| ( [ AddLocation Earth ], Narrate "AddLocation Earth" )
                , test "won't add a location twice"
                    <| \() ->
                        Expect.equal [ Earth, Moon ]
                            <| .knownLocations
                            <| (\newState -> advanceStory "Earth" newState <| ( [ AddLocation Earth ], Narrate "AddLocation Earth" ))
                            <| (\newState -> advanceStory "Earth" newState <| ( [ AddLocation Earth ], Narrate "AddLocation Earth" ))
                            <| advanceStory "Moon" init
                            <| ( [ AddLocation Moon ], Narrate "AddLocation Moon" )
                ]
            , describe "RemoveLocation"
                [ test "removes location from known locations without chaning order"
                    <| \() ->
                        Expect.equal [ Mars, Moon ]
                            <| .knownLocations
                            <| (\newState -> advanceStory "Earth" newState <| ( [ RemoveLocation Earth ], Narrate "RemoveLocation Earth" ))
                            <| (\newState -> advanceStory "Mars" newState <| ( [ AddLocation Mars ], Narrate "AddLocation Mars" ))
                            <| (\newState -> advanceStory "Earth" newState <| ( [ AddLocation Earth ], Narrate "AddLocation Earth" ))
                            <| advanceStory "Moon" init
                            <| ( [ AddLocation Moon ], Narrate "AddLocation Moon" )
                , test "does nothing if location is not in known locations"
                    <| \() ->
                        Expect.equal [ Mars, Moon ]
                            <| .knownLocations
                            <| (\newState -> advanceStory "Earth" newState <| ( [ RemoveLocation Earth ], Narrate "RemoveLocation Earth" ))
                            <| (\newState -> advanceStory "Mars" newState <| ( [ AddLocation Mars ], Narrate "AddLocation Mars" ))
                            <| advanceStory "Moon" init
                            <| ( [ AddLocation Moon ], Narrate "AddLocation Moon" )
                ]
            , describe "addProp"
                [ test "appends prop to items by location "
                    <| \() ->
                        Expect.equal [ ThingOne, ThingTwo ]
                            <| getPropsInCurrentLocation
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Earth ], Narrate "AddProp ThingTwo" ))
                            <| advanceStory "ThingOne" init
                            <| ( [ AddProp ThingOne Earth ], Narrate "AddProp ThingOne" )
                , test "wont duplicate items"
                    <| \() ->
                        Expect.equal [ ThingOne, ThingTwo ]
                            <| getPropsInCurrentLocation
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Earth ], Narrate "AddProp ThingTwo" ))
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Earth ], Narrate "AddProp ThingTwo" ))
                            <| advanceStory "ThingOne" init
                            <| ( [ AddProp ThingOne Earth ], Narrate "AddProp ThingOne" )
                ]
            , describe "removeProp"
                [ test "removes the prop from items by location"
                    <| \() ->
                        Expect.equal [ ThingTwo ]
                            <| getPropsInCurrentLocation
                            <| (\newState -> advanceStory "ThingOne" newState <| ( [ RemoveProp ThingOne Earth ], Narrate "RemoveProp ThingOne" ))
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Earth ], Narrate "AddProp ThingTwo" ))
                            <| advanceStory "ThingOne" init
                            <| ( [ AddProp ThingOne Earth ], Narrate "AddProp ThingOne" )
                , test "does nothing if the location does not have the prop"
                    <| \() ->
                        Expect.equal [ ThingOne ]
                            <| getPropsInCurrentLocation
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ RemoveProp ThingTwo Earth ], Narrate "RemoveProp ThingTwo" ))
                            <| advanceStory "ThingOne" init
                            <| ( [ AddProp ThingOne Earth ], Narrate "AddProp ThingOne" )
                , test "does nothing if the location is empty"
                    <| \() ->
                        Expect.equal []
                            <| getPropsInCurrentLocation
                            <| advanceStory "ThingOne" init
                            <| ( [ RemoveProp ThingOne Earth ], Narrate "RemoveProp ThingOne" )
                , test "leaves other locations alone"
                    <| \() ->
                        Expect.equal [ ThingOne, ThingTwo ]
                            <| getPropsInCurrentLocation
                            <| (\newState -> advanceStory "ThingOne" newState <| ( [ RemoveProp ThingOne Mars ], Narrate "RemoveProp ThingOne" ))
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Mars ], Narrate "AddProp ThingTwo" ))
                            <| (\newState -> advanceStory "ThingTwo" newState <| ( [ AddProp ThingTwo Earth ], Narrate "AddProp ThingTwo" ))
                            <| advanceStory "ThingOne" init
                            <| ( [ AddProp ThingOne Earth ], Narrate "AddProp ThingOne" )
                ]
            ]
        ]
