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

        -- , linkTests
        -- , queryTests
        ]


storeTests : Test
storeTests =
    test "basic store creation" <|
        \() ->
            Expect.equal (Dict.size store) (List.length entities)


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
                Expect.equal Nothing (getProperty "location" "strength" store)
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
        -- TODO
        []


queryTests : Test
queryTests =
    describe "querying"
        -- TODO
        []
