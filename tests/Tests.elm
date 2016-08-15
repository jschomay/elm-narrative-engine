module Tests exposing (suite)

import Tests.StoryStateTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.StoryStateTests.all
        ]
