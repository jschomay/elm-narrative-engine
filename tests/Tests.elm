module Tests exposing (suite)

import Tests.MechanicsTests
import Tests.StoryStateTests
import Tests.StoryRulesTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.MechanicsTests.all
        , Tests.StoryStateTests.all
        , Tests.StoryRulesTests.all
        ]
