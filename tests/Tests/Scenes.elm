module Tests.Scenes exposing (all)

import Engine.Scenes
import Test exposing (..)
import Expect
import Dict
import Types exposing (..)


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
                                    }
                              )
                            , ( "scene2", Dict.empty )
                            ]
                in
                    Expect.equal expected baseScenes
        ]


baseScenes : Scenes
baseScenes =
    Engine.Scenes.init
        [ ( "scene1"
          , [ ( "sample rule"
              , { interaction = WithAnything
                , conditions = []
                , changes = []
                }
              )
            ]
          )
        , ( "scene2", [] )
        ]
