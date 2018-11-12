module Rules exposing (all)

import Dict
import Expect
import Narrative.Rules exposing (..)
import Narrative.Store exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Rule tests"
        [ describe "findMatchingRule finds the right rule"
            [ test "from triggers" <|
                \() ->
                    let
                        rule1 =
                            ( "interact with item1"
                            , { trigger = TriggerMatching "item1"
                              , conditions = []
                              }
                            )

                        rule2 =
                            ( "interact with item2"
                            , { trigger = TriggerMatching "item2"
                              , conditions = []
                              }
                            )

                        rules =
                            Dict.fromList [ rule1, rule2 ]
                    in
                    Expect.equalLists
                        [ Just "interact with item1"
                        , Just "interact with item2"
                        ]
                        [ findMatchingRule rules "item1" store
                        , findMatchingRule rules "item2" store
                        ]
            , test "from conditions" <|
                \() ->
                    let
                        rule1 =
                            ( "does not match"
                            , { trigger = TriggerMatching "item1"
                              , conditions = [ EntityMatching "character1" [ HasLink "location" "the moon" ] ]
                              }
                            )

                        rule2 =
                            ( "does not match all conditions"
                            , { trigger = TriggerMatching "item1"
                              , conditions =
                                    [ EntityMatching "character1"
                                        [ HasLink "location" "location1"
                                        , HasTag "invisible"
                                        ]
                                    ]
                              }
                            )

                        rule3 =
                            ( "expected"
                            , { trigger = TriggerMatching "item1"
                              , conditions = [ EntityMatching "character1" [ HasLink "location" "location1" ] ]
                              }
                            )

                        rules =
                            Dict.fromList [ rule1, rule2, rule3 ]
                    in
                    Expect.equal (Just "expected") <|
                        findMatchingRule rules "item1" store
            ]
        , describe "finding the best match"
            [ test "all else equal, rules with more conditions win" <|
                \() ->
                    let
                        -- Note, without weighting, rules are sorted alphabetically by id, and then reversed
                        -- So without weighting implemented "less specific" would come before "expected"
                        rule1 =
                            ( "less specific"
                            , { trigger = TriggerMatching "item1"
                              , conditions = []
                              }
                            )

                        rule2 =
                            ( "expected"
                            , { trigger = TriggerMatching "item1"
                              , conditions = [ EntityMatching "character1" [ HasLink "location" "location1" ] ]
                              }
                            )

                        rules =
                            Dict.fromList [ rule1, rule2 ]
                    in
                    Expect.equal (Just "expected") <|
                        findMatchingRule rules "item1" store
            , test "all else equal, rules with more conditions win (based on condition queries)" <|
                \() ->
                    let
                        -- Note, without weighting, rules are sorted alphabetically by id, and then reversed
                        -- So without weighting implemented "less specific" would come before "expected"
                        rule1 =
                            ( "less specific"
                            , { trigger = TriggerMatching "item1"
                              , conditions = [ EntityMatching "character1" [ HasLink "location" "location1" ] ]
                              }
                            )

                        rule2 =
                            ( "expected"
                            , { trigger = TriggerMatching "item1"
                              , conditions =
                                    [ EntityMatching "character1"
                                        [ HasLink "location" "location1"
                                        , HasTag "friend"
                                        ]
                                    ]
                              }
                            )

                        rules =
                            Dict.fromList [ rule1, rule2 ]
                    in
                    Expect.equal (Just "expected") <|
                        findMatchingRule rules "item1" store
            ]
        ]



-- the test below apply to level-2 queries
--     -- the following would be good fuzz test candidates...
--     , test "ID-matching trigger rules beat query-matching trigger rules (regardless of conditions)" <|
--         \() ->
--             let
--                 rules =
--                     [ rule1, rule4, ruleX ]
--                 -- TODO test with ID-matching conditions too
--             in
--             Expect.equal (Just rule1) <|
--                 Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
--     , test "all else equal, ID-matching conditions beat query-matching conditions" <|
--         \() ->
--             let
--                 rules =
--                     [ rule1, rule4, ruleX ]
--             in
--             Expect.equal (Just rule1) <|
--                 Rules.findMatchingRule { story | rules = Dict.fromList rules } "x"
--     ]
-- ]


store =
    Narrative.Store.basic <|
        [ entity "item1"
            |> addTag "item"
        , entity "item2"
            |> addTag "item"
        , entity "character1"
            |> addTag "character"
            |> addTag "friend"
            |> setLink "location" "location1"
        , entity "location1"
            |> addTag "location"
        ]
