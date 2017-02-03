module Tests exposing (..)

import Tests.Manifest
import Tests.Rules
import Test exposing (..)


all : Test
all =
    describe "Suites"
        [ Tests.Manifest.all
        , Tests.Rules.all
        ]
