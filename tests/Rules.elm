module Rules exposing (all)

import Dict
import Expect
import Narrative.Rules exposing (..)
import Narrative.WorldModel exposing (..)
import Test exposing (..)


type alias Entity =
    NarrativeComponent { other : String }


emptyEntity : Entity
emptyEntity =
    { tags = emptyTags
    , stats = emptyStats
    , links = emptyLinks
    , other = "other"
    }


entity : String -> ( String, Entity )
entity id =
    ( id, emptyEntity )


store =
    Dict.fromList
        [ entity "item1"
            |> tag "item"
        , entity "item2"
            |> tag "item"
            |> link "location" "location2"
        , entity "character1"
            |> tag "character"
            |> tag "friend"
            |> link "location" "location1"
        , entity "location1"
            |> tag "location"
        , entity "location2"
            |> tag "location"
        ]


all : Test
all =
    describe "Rule tests"
        [ describe "findMatchingRule (triggers)" <|
            let
                rules =
                    [ ( "specific for item1"
                      , { trigger = Match "item1" []
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "specific for item2"
                      , { trigger = Match "item2" []
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "specific with query"
                      , { trigger = Match "character1" [ HasTag "friend" ]
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "generic"
                      , { trigger = MatchAny [ HasTag "location" ]
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "won't match (specific)"
                      , { trigger = Match "character1" [ HasTag "enemy" ]
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "won't match (generic)"
                      , { trigger = MatchAny [ HasTag "location", HasTag "locked" ]
                        , conditions = []
                        , changes = []
                        }
                      )
                    , ( "won't match (non-entity)"
                      , { trigger = Match "storyEvent" []
                        , conditions = []
                        , changes = []
                        }
                      )
                    ]
                        |> Dict.fromList
            in
            [ test "matching by id (tested twice to show match is not order dependent)" <|
                \() ->
                    Expect.equalLists
                        [ Just "specific for item1"
                        , Just "specific for item2"
                        ]
                        [ findMatchingRule "item1" rules store |> Maybe.map Tuple.first
                        , findMatchingRule "item2" rules store |> Maybe.map Tuple.first
                        ]
            , test "matching by id plus query" <|
                \() ->
                    Expect.equal
                        (Just "specific with query")
                        (findMatchingRule "character1" rules store |> Maybe.map Tuple.first)
            , test "generic (MatchAny)" <|
                \() ->
                    Expect.equal
                        (Just "generic")
                        (findMatchingRule "location1" rules store |> Maybe.map Tuple.first)
            , test "non-entity triggers" <|
                \() ->
                    Expect.equal
                        Nothing
                        (findMatchingRule "storyEvent" rules store |> Maybe.map Tuple.first)
            ]
        , describe "findMatchingRule (conditions)" <|
            let
                rules =
                    [ ( "does not match"
                      , { trigger = Match "item1" []
                        , conditions = [ Match "character1" [ HasLink "location" <| Match "the moon" [] ] ]
                        , changes = []
                        }
                      )
                    , ( "does not match all conditions"
                      , { trigger = Match "item1" []
                        , conditions =
                            [ Match "character1"
                                [ HasLink "location" <| Match "location1" []
                                , HasTag "invisible"
                                ]
                            ]
                        , changes = []
                        }
                      )
                    , ( "expected"
                      , { trigger = Match "item1" []
                        , conditions = [ Match "character1" [ HasLink "location" <| Match "location1" [] ] ]
                        , changes = []
                        }
                      )
                    , ( "trigger won't match"
                      , { trigger = Match "character1" [ HasTag "non-existant" ]
                        , conditions = [ Match "item1" [ HasTag "item" ] ]
                        , changes = []
                        }
                      )
                    , ( "generic"
                      , { trigger = Match "location1" []
                        , conditions = [ MatchAny [ HasTag "friend" ] ]
                        , changes = []
                        }
                      )
                    , ( "generic does not match"
                      , { trigger = Match "location1" []
                        , conditions = [ MatchAny [ HasTag "enemy" ] ]
                        , changes = []
                        }
                      )
                    , ( "match trigger"
                      , { trigger = MatchAny [ HasTag "location" ]
                        , conditions =
                            [ MatchAny [ HasLink "location" <| Match "$" [] ]
                            , Match "item2" [ HasLink "location" <| Match "$" [] ]
                            ]
                        , changes = []
                        }
                      )
                    ]
                        |> Dict.fromList
            in
            [ test "from specific conditions" <|
                \() ->
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "using '$' to reference trigger" <|
                \() ->
                    Expect.equal (Just "match trigger") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "location2" rules store
            , test "never matches if trigger doesn't match" <|
                \() ->
                    Expect.equal Nothing <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "character1" rules store
            , test "from generic conditions" <|
                \() ->
                    Expect.equal (Just "generic") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "location1" rules store
            ]
        , describe "finding the best match"
            [ test "with no weighting, rule in reverse alphabetical order wins" <|
                \() ->
                    let
                        rules =
                            [ ( "a"
                              , { trigger = Match "item1" []
                                , conditions = []
                                , changes = []
                                }
                              )
                            , ( "z"
                              , { trigger = Match "item1" []
                                , conditions = []
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList
                    in
                    Expect.equal (Just "z") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "all else equal, rules with more conditions win" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = Match "item1" []
                                , conditions = []
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = Match "item1" []
                                , conditions = [ Match "character1" [ HasLink "location" <| Match "location1" [] ] ]
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "all else equal, rules with more queries win" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = Match "item1" []
                                , conditions =
                                    [ Match "character1"
                                        [ HasLink "location" <| Match "location1" []
                                        ]
                                    ]
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = Match "item1" []
                                , conditions =
                                    [ Match "character1"
                                        [ HasLink "location" <| Match "location1" []
                                        , HasTag "friend"
                                        ]
                                    ]
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList

                        -- x =
                        --     Debug.log "" <| List.map weight <| Dict.values rules
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "all else equal, specific trigger matches with the most queries win" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = Match "item1" []
                                , conditions = []
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = Match "item1" [ HasTag "item" ]
                                , conditions = []
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "all else equal, generic trigger matches with the most queries win" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = MatchAny []
                                , conditions = []
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = MatchAny [ HasTag "item" ]
                                , conditions = []
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "all else equal, specific condition matches beat generic condition matches" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = Match "item1" []
                                , conditions =
                                    [ MatchAny [ HasTag "item" ]
                                    ]
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = Match "item1" []
                                , conditions =
                                    [ Match "item2" [ HasTag "item" ]
                                    ]
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            , test "specific trigger matches always beat generic trigger matches (regardless of number of queries or conditions)" <|
                \() ->
                    let
                        rules =
                            [ ( "z less specific"
                              , { trigger = MatchAny [ HasTag "item" ]
                                , conditions =
                                    [ Match "character1"
                                        [ HasLink "location" <| Match "location1" []
                                        , HasTag "friend"
                                        ]
                                    , Match "item2" [ HasTag "item" ]
                                    , Match "location1" [ HasTag "location" ]
                                    ]
                                , changes = []
                                }
                              )
                            , ( "expected"
                              , { trigger = Match "item1" []
                                , conditions = []
                                , changes = []
                                }
                              )
                            ]
                                |> Dict.fromList

                        -- x =
                        --     Debug.log "" <| List.map weight <| Dict.values rules
                    in
                    Expect.equal (Just "expected") <|
                        Maybe.map Tuple.first <|
                            findMatchingRule "item1" rules store
            ]
        ]
