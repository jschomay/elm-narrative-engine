module Tests exposing (suite)

import Tests.EngineTests
import Tests.StoryStateTests
import Tests.StoryRulesTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.EngineTests.all
        , Tests.StoryStateTests.all
        , Tests.StoryRulesTests.all
        ]
