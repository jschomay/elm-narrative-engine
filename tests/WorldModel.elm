module WorldModel exposing (all)

import Dict
import Expect
import Narrative.WorldModel exposing (..)
import Test exposing (..)


entities =
    [ entity "item1"
        |> addTag "item"
        |> addTag "special"
    , entity "item2"
        |> addTag "item"
        |> setLink "heldBy" "character1"
    , entity "character1"
        |> addTag "character"
        |> setStat "strength" 5
        |> setLink "holding" "item1"
        |> setLink "locatedIn" "location1"
    , entity "character2"
        |> addTag "character"
        |> setStat "strength" 2
    , entity "location1"
        |> addTag "location"
    ]


store =
    startingState entities


all : Test
all =
    describe "store tests"
        [ storeTests
        , tagsTests
        , statTests
        , linkTests
        , queryTests
        ]


storeTests : Test
storeTests =
    describe "store"
        [ test "creation" <|
            \() ->
                Expect.equal (Dict.size store) (List.length entities)
        , describe "updating"
            [ test "startingState" <|
                \() ->
                    Expect.true "update didn't work"
                        (update "item1" (addTag "updated") store |> hasTag "item1" "updated")
            , test "removeTag" <|
                \() ->
                    Expect.false "update didn't work"
                        (update "item1" (removeTag "special") store |> hasTag "item1" "special")
            , test "incStat" <|
                \() ->
                    Expect.equal (Just 7)
                        (update "character1" (incStat "strength" 2) store |> getStat "character1" "strength")
            , test "decStat" <|
                \() ->
                    Expect.equal (Just 3)
                        (update "character1" (decStat "strength" 2) store |> getStat "character1" "strength")
            , test "removeTag when tag not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal store
                        (update "item1" (removeTag "notPresent") store)
            , test "removeTag when entity not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal store
                        (update "notPresent" (removeTag "special") store)
            , test "via applyChanges" <|
                \() ->
                    Expect.true "changes did not apply correctly"
                        (store
                            |> applyChanges
                                [ AddTag "item1" "extraSpecial"
                                , SetLink "item1" "heldBy" "character1"
                                ]
                            |> assert "item1"
                                [ HasTag "extraSpecial"
                                , HasLink "heldBy" "character1"
                                ]
                        )
            ]
        ]


tagsTests : Test
tagsTests =
    describe "tags"
        [ test "hasTag true" <|
            \() ->
                Expect.true "missing tag" (hasTag "item1" "special" store)
        , test "hasTag false" <|
            \() ->
                Expect.false "didn't expect tag" (hasTag "item2" "special" store)
        ]


statTests : Test
statTests =
    describe "stat"
        [ test "getStat exists" <|
            \() ->
                Expect.equal (Just 5) (getStat "character1" "strength" store)
        , test "getStat does not exists" <|
            \() ->
                Expect.equal Nothing (getStat "location" "strength" store)
        , test "hasStat (EQ) true" <|
            \() ->
                Expect.true "expected stat" (hasStat "character1" "strength" EQ 5 store)
        , test "hasStat false no key" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "location" "strength" EQ 5 store)
        , test "hasStat false (EQ) wrong value" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "character1" "strength" EQ 1 store)
        , test "hasStat (GT) true" <|
            \() ->
                Expect.true "expected stat" (hasStat "character1" "strength" GT 4 store)
        , test "hasStat (GT) false" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "character1" "strength" GT 6 store)
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "getLink exists" <|
            \() ->
                Expect.equal (Just "item1") (getLink "character1" "holding" store)
        , test "getLink does not exists" <|
            \() ->
                Expect.equal Nothing (getLink "character1" "knowsAbout" store)
        , test "getLink does not exist 2" <|
            \() ->
                Expect.equal Nothing (getLink "item" "holding" store)
        , test "hasLink true" <|
            \() ->
                Expect.true "expected link" (hasLink "character1" "holding" "item1" store)
        , test "hasLink false" <|
            \() ->
                Expect.false "wrong value" (hasLink "character1" "holding" "location" store)
        , test "hasLink false 2" <|
            \() ->
                Expect.false "wrong key" (hasLink "character1" "knowsAbout" "item1" store)
        ]


queryTests : Test
queryTests =
    describe "querying"
        [ test "query tag - query items" <|
            \() ->
                Expect.equal [ "item1", "item2" ] <|
                    List.sort <|
                        query [ HasTag "item" ] store
        , test "query stat - strong characters" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    query [ HasTag "character", HasStat "strength" GT 3 ] store
        , test "query link - characters in location" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    query [ HasTag "character", HasLink "locatedIn" "location1" ] store
        , test "empty result" <|
            \() ->
                Expect.equal [] <|
                    query [ HasTag "other" ] store
        , test "assert positive" <|
            \() ->
                Expect.true "should be true" <|
                    assert "item1" [ HasTag "special" ] store
        , test "assert negative" <|
            \() ->
                Expect.false "should be false" <|
                    assert "item1" [ HasTag "extraSpecial" ] store
        , test "assert negative - not found" <|
            \() ->
                Expect.false "should not be found" <|
                    assert "item99" [ HasTag "item" ] store
        , test "not with assert" <|
            \() ->
                Expect.true "should be true" <|
                    assert "item1" [ Not (HasTag "ExtraSpecial") ] store
        , test "not with query" <|
            \() ->
                Expect.equal [ "item1" ] <|
                    query [ HasTag "item", Not (HasLink "heldBy" "character1") ] store
        ]
