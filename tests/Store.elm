module Store exposing (all)

import Dict
import Engine.Store exposing (..)
import Expect
import Test exposing (..)
import Types exposing (..)


entities =
    [ entity "item1"
        |> addTag "item"
        |> addTag "special"
        |> setProperty "weight" "heavy"
    , entity "item2"
        |> addTag "item"
    , entity "character"
        |> setStat "strength" 5
        |> setLink "holding" "item1"
    , entity "location"
    ]


store =
    initStore entities


all : Test
all =
    describe "store tests"
        [ storeTests
        , tagsTests
        , propertiesTests
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
            [ test "basic" <|
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
                        (update "character" (incStat "strength" 2) store |> getStat "character" "strength")
            , test "decStat" <|
                \() ->
                    Expect.equal (Just 3)
                        (update "character" (decStat "strength" 2) store |> getStat "character" "strength")
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


propertiesTests : Test
propertiesTests =
    describe "propertie"
        [ test "getProperty exists" <|
            \() ->
                Expect.equal (Just "heavy") (getProperty "item1" "weight" store)
        , test "getProperty does not exists" <|
            \() ->
                Expect.equal Nothing (getProperty "item2" "weight" store)
        , test "hasProperty true" <|
            \() ->
                Expect.true "expected property" (hasProperty "item1" "weight" "heavy" store)
        , test "hasProperty false no key" <|
            \() ->
                Expect.false "didn't expect property" (hasProperty "item1" "weight" "light" store)
        , test "hasProperty false wrong value" <|
            \() ->
                Expect.false "didn't expect property" (hasProperty "item2" "weight" "light" store)
        ]


statTests : Test
statTests =
    describe "stat"
        [ test "getStat exists" <|
            \() ->
                Expect.equal (Just 5) (getStat "character" "strength" store)
        , test "getStat does not exists" <|
            \() ->
                Expect.equal Nothing (getStat "location" "strength" store)
        , test "hasStat (EQ) true" <|
            \() ->
                Expect.true "expected stat" (hasStat "character" "strength" EQ 5 store)
        , test "hasStat false no key" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "location" "strength" EQ 5 store)
        , test "hasStat false (EQ) wrong value" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "character" "strength" EQ 1 store)
        , test "hasStat (GT) true" <|
            \() ->
                Expect.true "expected stat" (hasStat "character" "strength" GT 4 store)
        , test "hasStat (GT) false" <|
            \() ->
                Expect.false "didn't expect stat" (hasStat "character" "strength" GT 6 store)
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "getLink exists" <|
            \() ->
                Expect.equal (Just "item1") (getLink "character" "holding" store)
        , test "getLink does not exists" <|
            \() ->
                Expect.equal Nothing (getLink "character" "knowsAbout" store)
        , test "getLink does not exist 2" <|
            \() ->
                Expect.equal Nothing (getLink "item" "holding" store)
        , test "hasLink true" <|
            \() ->
                Expect.true "expected link" (hasLink "character" "holding" "item1" store)
        , test "hasLink false" <|
            \() ->
                Expect.false "wrong value" (hasLink "character" "holding" "location" store)
        , test "hasLink false 2" <|
            \() ->
                Expect.false "wrong key" (hasLink "character" "knowsAbout" "item1" store)
        ]


queryTests : Test
queryTests =
    skip <|
        describe "querying"
            []
