module Tests exposing (suite)

import Tests.StoryStateTests
import Tests.StoryRulesTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.StoryStateTests.all
        , Tests.StoryRulesTests.all
        ]
