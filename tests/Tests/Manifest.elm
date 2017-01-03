module Tests.Manifest exposing (all)

import Engine.Manifest
import Test exposing (..)
import Expect
import Dict
import Types exposing (..)


all : Test
all =
    describe "Manifest"
        [ test "init" <|
            \() ->
                let
                    expected =
                        Dict.fromList
                            [ ( "item1", Engine.Manifest.item <| attrs "item1" )
                            , ( "item2", Engine.Manifest.item <| attrs "item2" )
                            , ( "location1", Engine.Manifest.location <| attrs "location1" )
                            , ( "location2", Engine.Manifest.location <| attrs "location2" )
                            , ( "character1", Engine.Manifest.character <| attrs "character1" )
                            , ( "character2", Engine.Manifest.character <| attrs "character2" )
                            ]
                in
                    Expect.equal expected baseManifest
        , describe "getters"
            [ test "getInventory" <|
                \() ->
                    let
                        manifest =
                            Engine.Manifest.update (MoveItemToInventory "item2") baseManifest
                    in
                        Expect.equal (Engine.Manifest.getItemsInInventory manifest) [ ( "item2", attrs "item2" ) ]
            , test "getLocations" <|
                \() ->
                    let
                        manifest =
                            Engine.Manifest.update (AddLocation "location2") baseManifest
                    in
                        Expect.equal (Engine.Manifest.getLocations manifest) [ ( "location2", attrs "location2" ) ]
            , test "getCharactersInLocation" <|
                \() ->
                    let
                        manifest =
                            baseManifest
                                |> Engine.Manifest.update (MoveCharacterToLocation "character1" "location1")
                                |> Engine.Manifest.update (MoveCharacterToLocation "character1" "location2")
                    in
                        Expect.equal (Engine.Manifest.getCharactersInCurrentLocation "location2" manifest)
                            [ ( "character1", attrs "character1" ) ]
            , test "getItemsInCurrentLocation" <|
                \() ->
                    let
                        manifest =
                            baseManifest
                                |> Engine.Manifest.update (MoveItemToLocation "item1" "location1")
                                |> Engine.Manifest.update (MoveItemToLocation "item2" "location2")
                    in
                        Expect.equal (Engine.Manifest.getItemsInCurrentLocation "location2" manifest)
                            [ ( "item2", attrs "item2" ) ]
            ]
        , describe "matchers"
            [ test "itemIsInInventory" <|
                \() ->
                    let
                        manifest =
                            baseManifest
                                |> Engine.Manifest.update (MoveItemToInventory "item2")
                    in
                        Expect.equal (Engine.Manifest.itemIsInInventory "item2" manifest) True
            , test "characterIsInLocation" <|
                \() ->
                    let
                        manifest =
                            baseManifest
                                |> Engine.Manifest.update (MoveCharacterToLocation "character1" "location1")
                                |> Engine.Manifest.update (MoveTo "location1")
                    in
                        Expect.equal (Engine.Manifest.characterIsInLocation "character1" "location1" manifest) True
            , test "itemIsInLocation" <|
                \() ->
                    let
                        manifest =
                            baseManifest
                                |> Engine.Manifest.update (MoveItemToLocation "item1" "location1")
                                |> Engine.Manifest.update (MoveTo "location1")
                    in
                        Expect.equal (Engine.Manifest.itemIsInLocation "item1" "location1" manifest) True
            ]
        ]


attrs name =
    Attributes name "description"


items =
    [ ( "item1", attrs "item1" )
    , ( "item2", attrs "item2" )
    ]


locations =
    [ ( "location1", attrs "location1" )
    , ( "location2", attrs "location2" )
    ]


characters =
    [ ( "character1", attrs "character1" )
    , ( "character2", attrs "character2" )
    ]


baseManifest =
    Engine.Manifest.init { items = items, locations = locations, characters = characters }
