module Core.WorldModelTest exposing (all)

import Dict exposing (Dict)
import Expect
import NarrativeEngine.Core.WorldModel exposing (..)
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
            |> link "belongsTo" "character2"
            |> link "origin" "location1"
        , entity "item2"
            |> tag "item"
            |> link "heldBy" "character1"
        , entity "character1"
            |> tag "character"
            |> stat "strength" 5
            |> link "holding" "item1"
            |> link "locatedIn" "location1"
            |> link "badLink" "unknownID"
        , entity "character2"
            |> tag "character"
            |> stat "strength" 2
            |> stat "armor" 5
            |> link "locatedIn" "location1"
        , entity "location1"
            |> tag "location"
        , entity "location2"
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


{-| helper function for "assert" syntax"
-}
assert id queries store =
    query (Match id queries) store |> List.isEmpty |> not


storeTests : Test
storeTests =
    describe "store"
        [ describe "applyChanges/assert"
            [ test "AddTag" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges [ Update "item1" [ AddTag "updated" ] ] "trigger" worldModel |> assert "item1" [ HasTag "updated" ])
            , test "RemoveTag" <|
                \() ->
                    Expect.false "update didn't work"
                        (applyChanges [ Update "item1" [ RemoveTag "special" ] ] "trigger" worldModel |> assert "item1" [ HasTag "special" ])
            , test "IncStat" <|
                \() ->
                    Expect.equal (Just 7)
                        (applyChanges [ Update "character1" [ IncStat "strength" 2 ] ] "trigger" worldModel |> getStat "character1" "strength")
            , test "IncStat with unset stats assumes 0" <|
                \() ->
                    Expect.equal (Just 1)
                        (applyChanges [ Update "character1" [ IncStat "unsetStat" 1 ] ] "trigger" worldModel |> getStat "character1" "unsetStat")
            , test "DecStat" <|
                \() ->
                    Expect.equal (Just 3)
                        (applyChanges [ Update "character1" [ DecStat "strength" 2 ] ] "trigger" worldModel |> getStat "character1" "strength")
            , test "DecStat with unset stats assumes 0" <|
                \() ->
                    Expect.equal (Just -1)
                        (applyChanges [ Update "character1" [ DecStat "unsetStat" 1 ] ] "trigger" worldModel |> getStat "character1" "unsetStat")
            , test "SetStat" <|
                \() ->
                    Expect.equal (Just 9)
                        (applyChanges [ Update "character1" [ SetStat "strength" 9 ] ] "trigger" worldModel |> getStat "character1" "strength")
            , test "SetLink" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges [ Update "item2" [ SetLink "heldBy" "character2" ] ] "trigger" worldModel
                            |> assert "item2"
                                [ HasLink "heldBy" <| Match "character2" []
                                , Not (HasLink "heldBy" <| Match "character1" [])
                                ]
                        )
            , test "RemoveTag when tag not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal worldModel
                        (applyChanges [ Update "item1" [ RemoveTag "notPresent" ] ] "trigger" worldModel)
            , test "RemoveTag when entity not present does nothing" <|
                -- TODO should it do something else?
                \() ->
                    Expect.equal worldModel
                        (applyChanges [ Update "notPresent" [ RemoveTag "special" ] ] "trigger" worldModel)
            , test "with multiple changes" <|
                \() ->
                    Expect.true "changes did not apply correctly"
                        (applyChanges
                            [ Update "item1"
                                [ AddTag "extraSpecial"
                                , SetLink "heldBy" "character1"
                                ]
                            ]
                            "trigger"
                            worldModel
                            |> assert "item1"
                                [ HasTag "extraSpecial"
                                , HasLink "heldBy" <| Match "character1" []
                                ]
                        )
            , test "on multiple entities" <|
                \() ->
                    Expect.all
                        [ assert "item1" [ HasTag "updated" ] >> Expect.true "update didn't work"
                        , assert "item2" [ HasTag "updated" ] >> Expect.true "update didn't work"
                        ]
                        (applyChanges
                            [ Update "item1" [ AddTag "updated" ]
                            , Update "item2" [ AddTag "updated" ]
                            ]
                            "trigger"
                            worldModel
                        )
            , test "UpdateAll" <|
                \() ->
                    Expect.all
                        [ assert "item1" [ HasTag "updated", HasTag "again" ] >> Expect.true "update didn't work"
                        , assert "item2" [ HasTag "updated", HasTag "again" ] >> Expect.true "update didn't work"
                        ]
                        (applyChanges
                            [ UpdateAll [ HasTag "item" ] [ AddTag "updated", AddTag "again" ] ]
                            "trigger"
                            worldModel
                        )
            , test "update trigger (as subject)" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges
                            [ Update "$" [ AddTag "updated" ] ]
                            "item1"
                            worldModel
                            |> assert "item1" [ HasTag "updated" ]
                        )
            , test "update trigger (in link)" <|
                \() ->
                    Expect.true "update didn't work"
                        (applyChanges
                            [ Update "character1" [ SetLink "locatedIn" "$" ] ]
                            "location2"
                            worldModel
                            |> assert "character1" [ HasLink "locatedIn" <| Match "location2" [] ]
                        )
            , test "update trigger (in link with UpdateAll)" <|
                \() ->
                    Expect.all
                        [ assert "item1" [ HasLink "heldBy" <| Match "character2" [] ] >> Expect.true "update didn't work"
                        , assert "item2" [ HasLink "heldBy" <| Match "character2" [] ] >> Expect.true "update didn't work"
                        ]
                        (applyChanges
                            [ UpdateAll [ HasTag "item" ] [ SetLink "heldBy" "$" ] ]
                            "character2"
                            worldModel
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
                Expect.true "expected stat" (assert "character1" [ HasStat "strength" EQ <| SpecificStat 5 ] worldModel)
        , test "hasStat false no key" <|
            \() ->
                Expect.false "didn't expect stat" (assert "location" [ HasStat "strength" EQ <| SpecificStat 5 ] worldModel)
        , test "hasStat false (EQ) wrong value" <|
            \() ->
                Expect.false "didn't expect stat" (assert "character1" [ HasStat "strength" EQ <| SpecificStat 1 ] worldModel)
        , test "hasStat (GT) true" <|
            \() ->
                Expect.true "expected stat" (assert "character1" [ HasStat "strength" GT <| SpecificStat 4 ] worldModel)
        , test "hasStat (GT) false" <|
            \() ->
                Expect.false "didn't expect stat" (assert "character1" [ HasStat "strength" GT <| SpecificStat 6 ] worldModel)
        , test "unknown stat defaults to 0" <|
            \() ->
                Expect.true "unknown stat should have been 0"
                    (assert "character1"
                        [ HasStat "unsetStat" LT <| SpecificStat 1
                        , HasStat "unsetStat" GT <| SpecificStat -1
                        ]
                        worldModel
                    )
        , test "hasStat compare true" <|
            \() ->
                Expect.true "character1 strength should equal character2 armor" (assert "character1" [ HasStat "strength" EQ <| CompareStat "character2" "armor" ] worldModel)
        , test "hasStat compare false" <|
            \() ->
                Expect.false "character1 strength should equal character2 armor" (assert "character1" [ HasStat "strength" GT <| CompareStat "character2" "armor" ] worldModel)
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "getLink exists" <|
            \() ->
                Expect.equal (Just "item1") (getLink "character1" "holding" worldModel)
        , test "getLink key does not exists" <|
            \() ->
                Expect.equal Nothing (getLink "character1" "unknownKey" worldModel)
        , test "getLink value does not exist" <|
            \() ->
                Expect.equal Nothing (getLink "character1" "badLink" worldModel)
        , test "getLink entity does not exist" <|
            \() ->
                Expect.equal Nothing (getLink "unknownEntity" "holding" worldModel)
        , test "hasLink true" <|
            \() ->
                Expect.true "expected link" (assert "character1" [ HasLink "holding" <| Match "item1" [] ] worldModel)
        , test "hasLink false" <|
            \() ->
                Expect.false "wrong value" (assert "character1" [ HasLink "holding" <| Match "location1" [] ] worldModel)
        , test "hasLink false (bad key)" <|
            \() ->
                Expect.false "wrong key" (assert "character1" [ HasLink "badKey" <| Match "item1" [] ] worldModel)
        , test "hasLink false (bad value)" <|
            \() ->
                Expect.false "wrong key" (assert "character1" [ HasLink "badLink" <| Match "unknownID" [] ] worldModel)
        , test "hasLink with queries true" <|
            \() ->
                Expect.true "queries match" <|
                    assert "character1" [ HasLink "holding" (Match "item1" [ HasTag "item" ]) ] worldModel
        , test "hasLink with queries false" <|
            \() ->
                Expect.false "queries don't match" <|
                    assert "character1" [ HasLink "holding" (Match "item1" [ HasTag "location" ]) ] worldModel
        , test "hasLink generic true" <|
            \() ->
                Expect.true "existing relation" <|
                    assert "character1" [ HasLink "holding" (MatchAny [ HasTag "item" ]) ] worldModel
        , test "hasLink generic false" <|
            \() ->
                Expect.false "no relation" <|
                    assert "character1" [ HasLink "holding" (MatchAny [ HasTag "location" ]) ] worldModel
        , test "hasLink chained true" <|
            \() ->
                Expect.true "valid chain" <|
                    assert "character1"
                        [ HasLink "holding"
                            (Match "item1"
                                [ HasLink "belongsTo"
                                    (Match "character2" [])
                                ]
                            )
                        ]
                        worldModel
        , test "hasLink chained false" <|
            \() ->
                Expect.false "not valid chain" <|
                    assert "character1"
                        [ HasLink "holding"
                            (Match "item1"
                                [ HasLink "belongsTo"
                                    (Match "character1" [])
                                ]
                            )
                        ]
                        worldModel
        , skip <|
            test "hasLink compare" <|
                \() ->
                    Expect.true "character1 and 2 should have same location" (assert "character1" [] worldModel)
        ]


queryTests : Test
queryTests =
    describe "querying"
        [ test "query tag - query items" <|
            \() ->
                Expect.equal [ "item1", "item2" ] <|
                    List.sort <|
                        List.map Tuple.first <|
                            query (MatchAny [ HasTag "item" ]) worldModel
        , test "query stat - strong characters" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    List.map Tuple.first <|
                        query (MatchAny [ HasTag "character", HasStat "strength" GT <| SpecificStat 3 ]) worldModel
        , test "query stat compare - characters stronger than character2" <|
            \() ->
                Expect.equal [ "character1" ] <|
                    List.map Tuple.first <|
                        query (MatchAny [ HasTag "character", HasStat "strength" GT <| CompareStat "character2" "strength" ]) worldModel
        , test "query link - characters in location" <|
            \() ->
                Expect.equal [ "character1", "character2" ] <|
                    List.map Tuple.first <|
                        query (MatchAny [ HasTag "character", HasLink "locatedIn" <| Match "location1" [] ]) worldModel
        , todo "query link compare - characters in same location as item1's origin"
        , test "empty result" <|
            \() ->
                Expect.equal [] <|
                    List.map Tuple.first <|
                        query (MatchAny [ HasTag "other" ]) worldModel
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
                    List.map Tuple.first <|
                        query (MatchAny [ HasTag "item", Not (HasLink "heldBy" <| Match "character1" []) ]) worldModel
        , test "replacing $" <|
            \() ->
                Match "$" [ HasLink "link" (Match "$" []) ]
                    |> replaceTrigger "myId"
                    |> Expect.equal (Match "myId" [ HasLink "link" (Match "myId" []) ])
        ]
