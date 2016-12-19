module Tests exposing (..)

import Tests.Manifest
import Tests.Scenes
import Test exposing (..)


all : Test
all =
    describe "Suites"
        [ Tests.Manifest.all
        , Tests.Scenes.all
        ]
