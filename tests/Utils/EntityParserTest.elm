module Utils.EntityParserTest exposing (all)

import Expect
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.EntityParser exposing (parseEntity)
import NarrativeEngine.Utils.Helpers exposing (parseMultiple)
import Result
import Test exposing (..)


all =
    describe "parsing entitties"
        [ worldDefinition ]


makeEntity id =
    ( id
    , { tags = emptyTags
      , stats = emptyStats
      , links = emptyLinks
      }
    )


worldDefinition =
    describe "world definition"
        [ test "just id" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE" |> Ok)
                    (parseEntity "CAVE_ENTRANCE")
        , test "parses the whole thing (extra after id)" <|
            \() ->
                shouldFail "has extra chars after id"
                    -- `;` isn't a valid prop char
                    (parseEntity "CAVE_ENTRANCE;drop table")
        , test "parses the whole thing (extra after prop)" <|
            \() ->
                shouldFail "has extra chars after prop"
                    -- `;` isn't a valid prop char
                    (parseEntity "CAVE_ENTRANCE.location;drop table")
        , test "spaces in id not allowed" <|
            \() ->
                shouldFail "can't use spaces"
                    (parseEntity "CAVE ENTRANCE")
        , test "spaces in prop not allowed" <|
            \() ->
                shouldFail "can't use spaces"
                    (parseEntity "CAVE_ENTRANCE.is dark")
        , test "with one tag" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> tag "location"
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.location")
        , test "with multiple tags" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> tag "location"
                        |> tag "dark"
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.location.dark")
        , test "with one stat" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> stat "illumination" 4
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.illumination=4")
        , test "with one negative stat" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> stat "illumination" -4
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.illumination=-4")
        , test "improper stat" <|
            \() ->
                shouldFail "improper stat (char after int)"
                    (parseEntity "CAVE_ENTRANCE.illumination=4x")
        , test "tag followed by stat" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> stat "illumination" 4
                        |> tag "scary"
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.scary.illumination=4")
        , test "stat followed by tag" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> stat "illumination" 4
                        |> tag "scary"
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.illumination=4.scary")
        , test "multiple stats" <|
            \() ->
                Expect.equal
                    (makeEntity "CAVE_ENTRANCE"
                        |> stat "illumination" 4
                        |> stat "temp" 32
                        |> Ok
                    )
                    (parseEntity "CAVE_ENTRANCE.illumination=4.temp=32")
        , test "with one link" <|
            \() ->
                Expect.equal
                    (makeEntity "GOBLIN"
                        |> link "location" "CAVE"
                        |> Ok
                    )
                    (parseEntity "GOBLIN.location=CAVE")
        , test "all together" <|
            \() ->
                Expect.equal
                    (makeEntity "BAG_OF_GOLD"
                        |> tag "item"
                        |> tag "quest_item"
                        |> stat "value" 99
                        |> link "location" "CAVE"
                        |> link "guarded_by" "GOBLIN"
                        |> Ok
                    )
                    (parseEntity "BAG_OF_GOLD.item.quest_item.value=99.location=CAVE.guarded_by=GOBLIN")
        , test "spaced out" <|
            \() ->
                Expect.equal
                    (makeEntity "BAG_OF_GOLD"
                        |> tag "item"
                        |> stat "value" 99
                        |> link "location" "CAVE"
                        |> Ok
                    )
                    (parseEntity "BAG_OF_GOLD      .item  .value=99  .location=CAVE")
        , test "multi line" <|
            \() ->
                Expect.equal
                    (makeEntity "BAG_OF_GOLD"
                        |> tag "item"
                        |> stat "value" 99
                        |> link "location" "CAVE"
                        |> Ok
                    )
                    (parseEntity
                        """BAG_OF_GOLD
                                .item
                                .value=99
                                .location=CAVE"""
                    )
        , test "props can start with ints" <|
            \() ->
                Expect.equal
                    (makeEntity "A" |> tag "1st_place" |> Ok)
                    (parseEntity "A.1st_place")
        , test "props with special characters" <|
            \() ->
                Expect.equal
                    (makeEntity "A" |> tag "_:#" |> Ok)
                    (parseEntity "A._:#")
        , test "ids cannot start with ints" <|
            \() ->
                shouldFail "should fail because starts with int"
                    (parseEntity "1ST_BASE.location")
        , test "ids cannot start with ints (in links)" <|
            \() ->
                shouldFail "should fail because starts with int"
                    (parseEntity "PLAYER.location=1ST_BASE")
        , test "single letter id ok" <|
            \() ->
                Expect.equal
                    (makeEntity "A" |> Ok)
                    (parseEntity "A")
        ]


shouldFail message res =
    case res of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail message
