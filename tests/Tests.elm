module Tests exposing (suite)

import Tests.MechanicsTests
import Tests.StateTests
import Tests.RulesTests
import Test exposing (..)


suite : Test
suite =
    describe "Full suite"
        [ Tests.MechanicsTests.all
        , Tests.StateTests.all
        , Tests.RulesTests.all
        ]
