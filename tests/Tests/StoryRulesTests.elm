module Tests.StoryRulesTests exposing (all)

import Test exposing (..)
import Expect exposing (..)
import StoryState exposing (..)


type TestLocation
    = Earth
    | Moon
    | Mars


type TestScene
    = Begining
    | Middle
    | End


all : Test
all =
    describe "StoryRulesTests"
        [ describe "updateFromRules"
            [ test "..."
                <| \() ->
                      Expect.equal True True
            ]
        ]

-- always adds exactely one narration to storyline
-- only runs one rule
-- runs first matching rule

