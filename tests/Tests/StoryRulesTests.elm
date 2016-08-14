module Tests.StoryRulesTests exposing (all)

import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "general"
        [ test "test"
            <| \() ->
                Expect.equal True False
        ]
