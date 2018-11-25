module WorldModel exposing (all)

import Dict exposing (Dict)
import Expect
import Narrative.WorldModel exposing (..)
import Test exposing (..)


type alias DescriptionComponent a =
    { a | name : String, description : String }


type alias MyEntity =
    NarrativeComponent (DescriptionComponent {})


type alias MyWorldModel =
    Dict String MyEntity


emptyEntity : MyEntity
emptyEntity =
    { name = ""
    , description = ""
    , tags = emptyTags
    , stats = emptyStats
    , links = emptyLinks
    }


entity : String -> ( String, MyEntity )
entity id =
    ( id, emptyEntity )


worldModel : MyWorldModel
worldModel =
    Dict.fromList
        [ entity "item1"
            |> tag "item"
            |> tag "special"
        , entity "item2"
            |> tag "item"
            |> link "heldBy" "character1"
        , entity "character1"
            |> tag "character"
            |> stat "strength" 5
            |> link "holding" "item1"
            |> link "locatedIn" "location1"
        , entity "character2"
            |> tag "character"
            |> stat "strength" 2
        , entity "location1"
            |> tag "location"
        ]


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
        [ describe "applyChanges/assert"
            [ test "AddTag" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges [ AddTag "item1" "updated" ] worldModel |> assert "item1" [ HasTag "updated" ])
            , test "RemoveTag" <|
                \() ->
                    Expect.false "update didn't work"
                        (applyChanges [ RemoveTag "item1" "special" ] worldModel |> assert "item1" [ HasTag "special" ])
            , test "IncStat" <|
                \() ->
                    Expect.equal (Just 7)
                        (applyChanges [ IncStat "character1" "strength" 2 ] worldModel |> getStat "character1" "strength")
            , test "DecStat" <|
                \() ->
                    Expect.equal (Just 3)
                        (applyChanges [ DecStat "character1" "strength" 2 ] worldModel |> getStat "character1" "strength")
            , test "SetStat" <|
                \() ->
                    Expect.equal (Just 9)
                        (applyChanges [ SetStat "character1" "strength" 9 ] worldModel |> getStat "character1" "strength")
            , test "SetLink" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges [ SetLink "item2" "heldBy" "character2" ] worldModel
                            |> assert "item2"
                                [ HasLink "heldBy" "character2"
                                , Not (HasLink "heldBy" "character1")
                                ]
                        )
            , test "RemoveTag when tag not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal worldModel
                        (applyChanges [ RemoveTag "item1" "notPresent" ] worldModel)
            , test "RemoveTag when entity not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal worldModel
                        (applyChanges [ RemoveTag "notPresent" "special" ] worldModel)
            , test "with multiple changes" <|
                \() ->
                    Expect.true "changes did not apply correctly"
                        (worldModel
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
                Expect.true "missing tag" (assert "item1" [ HasTag "special" ] worldModel)
        , test "hasTag false" <|
            \() ->
                Expect.false "didn't expect tag" (assert "item2" [ HasTag "special" ] worldModel)
        ]


statTests : Test
statTests =
    describe "stat"
        [ test "getStat exists" <|
            \() ->
                Expect.equal (Just 5) (getStat "character1" "strength" worldModel)
        , test "getStat does not exists" <|
            \() ->
                Expect.equal Nothing (getStat "location" "strength" worldModel)
        , test "hasStat (EQ) true" <|
            \() ->
                Expect.true "expected stat" (assert "character1" [ HasStat "strength" EQ 5 ] worldModel)
        , test "hasStat false no key" <|
            \() ->
                Expect.false "didn't expect stat" (assert "location" [ HasStat "strength" EQ 5 ] worldModel)
        , test "hasStat false (EQ) wrong value" <|
            \() ->
                Expect.false "didn't expect stat" (assert "character1" [ HasStat "strength" EQ 1 ] worldModel)
        , test "hasStat (GT) true" <|
            \() ->
                Expect.true "expected stat" (assert "character1" [ HasStat "strength" GT 4 ] worldModel)
        , test "hasStat (GT) false" <|
            \() ->
                Expect.false "didn't expect stat" (assert "character1" [ HasStat "strength" GT 6 ] worldModel)
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "getLink exists" <|
            \() ->
                Expect.equal (Just "item1") (getLink "character1" "holding" worldModel)
        , test "getLink does not exists" <|
            \() ->
                Expect.equal Nothing (getLink "character1" "knowsAbout" worldModel)
        , test "getLink does not exist 2" <|
            \() ->
                Expect.equal Nothing (getLink "item" "holding" worldModel)
        , test "hasLink true" <|
            \() ->
                Expect.true "expected link" (assert "character1" [ HasLink "holding" "item1" ] worldModel)
        , test "hasLink false" <|
            \() ->
                Expect.false "wrong value" (assert "character1" [ HasLink "holding" "location" ] worldModel)
        , test "hasLink false 2" <|
            \() ->
                Expect.false "wrong key" (assert "character1" [ HasLink "knowsAbout" "item1" ] worldModel)
        ]


queryTests : Test
queryTests =
    describe "querying"
        [ test "query tag - query items" <|
            \() ->
                Expect.equal [ "item1", "item2" ] <|
                    List.sort <|
                        query [ HasTag "item" ] worldModel
        , test "query stat - strong characters" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    query [ HasTag "character", HasStat "strength" GT 3 ] worldModel
        , test "query link - characters in location" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    query [ HasTag "character", HasLink "locatedIn" "location1" ] worldModel
        , test "empty result" <|
            \() ->
                Expect.equal [] <|
                    query [ HasTag "other" ] worldModel
        , test "assert positive" <|
            \() ->
                Expect.true "should be true" <|
                    assert "item1" [ HasTag "special" ] worldModel
        , test "assert negative" <|
            \() ->
                Expect.false "should be false" <|
                    assert "item1" [ HasTag "extraSpecial" ] worldModel
        , test "assert negative - not found" <|
            \() ->
                Expect.false "should not be found" <|
                    assert "item99" [ HasTag "item" ] worldModel
        , test "not with assert" <|
            \() ->
                Expect.true "should be true" <|
                    assert "item1" [ Not (HasTag "ExtraSpecial") ] worldModel
        , test "not with query" <|
            \() ->
                Expect.equal [ "item1" ] <|
                    query [ HasTag "item", Not (HasLink "heldBy" "character1") ] worldModel
        ]
