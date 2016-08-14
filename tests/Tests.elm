module Tests exposing (suite)

import Tests.StoryRulesTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.StoryRulesTests.all
        ]
