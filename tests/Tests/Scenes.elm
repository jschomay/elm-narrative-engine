module Tests.Scenes exposing (all)

import Engine.Scenes
import Test exposing (..)
import Expect
import Dict
import Types exposing (..)
import List.Zipper


all : Test
all =
    describe "Scenes"
        [ test "init" <|
            \() ->
                let
                    expected =
                        Dict.fromList
                            [ ( "scene1"
                              , Dict.singleton "sample rule"
                                    { interaction = WithAnything
                                    , conditions = []
                                    , changes = []
                                    , narration = List.Zipper.fromList [ Just "narration1", Just "narration2" ] |> List.Zipper.withDefault Nothing
                                    }
                              )
                            , ( "scene2", Dict.empty )
                            ]
                in
                    Expect.equal expected baseScenes
        , test "update" <|
            \() ->
                Expect.equal (Just "narration2")
                    (baseScenes
                        |> Engine.Scenes.update "scene1" "sample rule"
                        |> Engine.Scenes.update "scene1" "sample rule"
                        |> Dict.get "scene1"
                        |> Maybe.andThen (Dict.get "sample rule")
                        |> Maybe.andThen Engine.Scenes.getNarration
                    )
        ]


baseScenes : Scenes
baseScenes =
    Engine.Scenes.init
        [ ( "scene1"
          , [ ( "sample rule"
              , { interaction = WithAnything
                , conditions = []
                , changes = []
                , narration = [ "narration1", "narration2" ]
                }
              )
            ]
          )
        , ( "scene2", [] )
        ]
