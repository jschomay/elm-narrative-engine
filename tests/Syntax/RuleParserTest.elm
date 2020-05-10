module Syntax.RuleParserTest exposing (all)

import Dict
import Expect
import NarrativeEngine.Core.Rules exposing (..)
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Syntax.Helpers exposing (parseMultiple)
import NarrativeEngine.Syntax.RuleParser exposing (parseChanges, parseMatcher, parseRule, parseRules)
import Result
import Test exposing (..)


all =
    describe "parsing rules"
        [ matchers
        , changesTest
        , multiple
        , parseRuleTest
        , parseRulesTest
        ]


matchers =
    describe "matchers"
        [ test "any" <|
            \() ->
                Expect.equal
                    (Ok <| MatchAny [])
                    (parseMatcher "*")
        , test "id" <|
            \() ->
                Expect.equal
                    (Ok <| Match "cave" [])
                    (parseMatcher "cave")
        , test "uses full input" <|
            \() ->
                shouldFail "didn't parse full input"
                    (parseMatcher "cave asdf*=$")
        , test "tag" <|
            \() ->
                Expect.equal
                    (Ok <| MatchAny [ HasTag "dark" ])
                    (parseMatcher "*.dark")

        --stats
        , test "stat =" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "fear" EQ <| SpecificStat 5 ])
                    (parseMatcher "PLAYER.fear=5")
        , test "stat >" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "fear" GT <| SpecificStat 5 ])
                    (parseMatcher "PLAYER.fear>5")
        , test "stat <" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "fear" LT <| SpecificStat 5 ])
                    (parseMatcher "PLAYER.fear<5")
        , test "stat compare =" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "strength" EQ (CompareStat "ENEMY" "armor") ])
                    (parseMatcher "PLAYER.strength=(stat ENEMY.armor)")
        , test "stat compare >" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "strength" GT (CompareStat "ENEMY" "armor") ])
                    (parseMatcher "PLAYER.strength>(stat ENEMY.armor)")
        , test "stat compare <" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "strength" LT (CompareStat "ENEMY" "armor") ])
                    (parseMatcher "PLAYER.strength<(stat ENEMY.armor)")
        , test "stat compare < with $" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasStat "strength" LT (CompareStat "$" "armor") ])
                    (parseMatcher "PLAYER.strength<(stat $.armor)")

        -- links
        , test "link just id" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (SpecificLink <| Match "CAVE" []) ])
                    (parseMatcher "PLAYER.location=CAVE")
        , test "link with $" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (SpecificLink <| Match "$" []) ])
                    (parseMatcher "PLAYER.location=$")
        , test "$ as selector (needed for conditional text)" <|
            \() ->
                Expect.equal
                    (Ok <| Match "$" [ HasTag "dark" ])
                    (parseMatcher "$.dark")
        , test "link missing parens" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasTag "dark", HasLink "location" (SpecificLink <| Match "CAVE" []) ])
                    (parseMatcher "PLAYER.location=CAVE.dark")
        , test "link with subquery" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (SpecificLink <| Match "CAVE" [ HasTag "dark" ]) ])
                    (parseMatcher "PLAYER.location=(CAVE.dark)")
        , test "link with subquery with MatchAny" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (SpecificLink <| MatchAny [ HasTag "dark", HasTag "location" ]) ])
                    (parseMatcher "PLAYER.location=(*.location.dark)")
        , test "link with subquery and extra tag" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasTag "blinded", HasLink "location" (SpecificLink <| Match "CAVE" [ HasTag "dark" ]) ])
                    (parseMatcher "PLAYER.location=(CAVE.dark).blinded")
        , test "link with nested subquery" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (SpecificLink <| MatchAny [ HasLink "homeTo" (SpecificLink <| Match "GOBLIN" []), HasTag "location" ]) ])
                    (parseMatcher "PLAYER.location=(*.location.homeTo=GOBLIN)")
        , test "link super nested" <|
            \() ->
                Expect.equal
                    (Ok <|
                        Match "PLAYER"
                            [ HasTag "scared"
                            , HasLink "location"
                                (SpecificLink <|
                                    MatchAny
                                        [ HasTag "dark"
                                        , HasLink "homeTo" (SpecificLink <| MatchAny [ HasTag "enemy" ])
                                        , HasTag "location"
                                        ]
                                )
                            ]
                    )
                    (parseMatcher "PLAYER.location=(*.location.homeTo=(*.enemy).dark).scared")
        , test "link compare" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (CompareLink "ENEMY" "home_world") ])
                    (parseMatcher "PLAYER.location=(link ENEMY.home_world)")
        , test "link compare with $" <|
            \() ->
                Expect.equal
                    (Ok <| Match "PLAYER" [ HasLink "location" (CompareLink "$" "home_world") ])
                    (parseMatcher "PLAYER.location=(link $.home_world)")

        -- other
        , test "all together" <|
            \() ->
                Expect.equal
                    (Ok <|
                        Match "A"
                            [ HasTag "tag3"
                            , HasLink "link2" (SpecificLink <| Match "C" [])
                            , HasLink "link1" (SpecificLink <| Match "B" [])
                            , HasStat "stat2" GT <| SpecificStat 2
                            , HasStat "stat1" EQ <| SpecificStat 1
                            , HasTag "tag2"
                            , HasTag "tag1"
                            ]
                    )
                    (parseMatcher "A.tag1.tag2.stat1=1.stat2>2.link1=B.link2=C.tag3")
        , test "not" <|
            \() ->
                Expect.equal
                    (Ok <| MatchAny [ HasTag "scary", Not (HasTag "dark"), HasTag "location" ])
                    (parseMatcher "*.location.!dark.scary")
        , test "not 2" <|
            \() ->
                Expect.equal
                    (Ok <| Match "TORCH" [ Not (HasLink "location" (SpecificLink <| Match "PLAYER" [])) ])
                    (parseMatcher "TORCH.!location=PLAYER")
        , test "spaces not allowed" <|
            \() ->
                shouldFail "spaces not allowed"
                    (parseMatcher "PLAYER .location=CAVE .fear>2 .blinded")
        , test "multiline" <|
            \() ->
                shouldFail "newlines not allowed"
                    (parseMatcher """PLAYER
                                        .location=CAVE
                                        .fear>2
                                        .blinded""")
        ]


changesTest =
    describe "changes"
        [ test "add tag" <|
            \() ->
                Expect.equal
                    (Ok <| Update "CAVE" [ AddTag "explored" ])
                    (parseChanges "CAVE.explored")
        , test "remove tag" <|
            \() ->
                Expect.equal
                    (Ok <| Update "GOBLIN" [ RemoveTag "sleeping" ])
                    (parseChanges "GOBLIN.-sleeping")
        , test "link" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ SetLink "location" <| SpecificLinkTarget "CAVE" ])
                    (parseChanges "PLAYER.location=CAVE")
        , test "link look up" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ SetLink "location" <| LookUpLinkTarget "SUSPECT" "hidehout" ])
                    (parseChanges "PLAYER.location=(link SUSPECT.hidehout)")
        , test "set stat" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ SetStat "fear" 9 ])
                    (parseChanges "PLAYER.fear=9")
        , test "inc stat" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ IncStat "fear" 1 ])
                    (parseChanges "PLAYER.fear+1")
        , test "dec stat" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ DecStat "fear" 1 ])
                    (parseChanges "PLAYER.fear-1")
        , test "all together" <|
            \() ->
                Expect.equal
                    (Ok <|
                        Update "PLAYER"
                            [ AddTag "blinded"
                            , IncStat "fear" 2
                            , RemoveTag "safe"
                            , SetLink "location" <| SpecificLinkTarget "CAVE"
                            ]
                    )
                    (parseChanges "PLAYER.location=CAVE.-safe.fear+2.blinded")
        , test "spaces" <|
            \() ->
                Expect.equal
                    (Ok <|
                        Update "PLAYER"
                            [ AddTag "blinded"
                            , SetLink "location" <| SpecificLinkTarget "CAVE"
                            ]
                    )
                    (parseChanges "PLAYER  .location=CAVE  .blinded")
        , test "multiline disabled" <|
            \() ->
                shouldFail "multiline not allowed for changes"
                    (parseChanges """PLAYER
                                        .location=CAVE
                                        .blinded""")
        , test "update trigger" <|
            \() ->
                Expect.equal
                    (Ok <| Update "$" [ AddTag "explored" ])
                    (parseChanges "$.explored")
        , test "update link to trigger" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ SetLink "location" <| SpecificLinkTarget "$" ])
                    (parseChanges "PLAYER.location=$")
        , test "update lookup link to trigger" <|
            \() ->
                Expect.equal
                    (Ok <| Update "PLAYER" [ SetLink "location" <| LookUpLinkTarget "$" "hidehout" ])
                    (parseChanges "PLAYER.location=(link $.hidehout)")
        , test "UpdateAll" <|
            \() ->
                Expect.equal
                    (Ok <| UpdateAll [ HasTag "suspect" ] [ RemoveTag "suspect" ])
                    (parseChanges "(*.suspect).-suspect")
        ]


multiple =
    describe "multiple"
        [ test "with parseMatcher (successful)" <|
            \() ->
                Expect.equal
                    (Ok <|
                        [ MatchAny [ HasLink "location" (SpecificLink <| Match "PLAYER" []), HasTag "light" ]
                        , Match "CAVE" [ HasTag "dark" ]
                        ]
                    )
                    (parseMultiple parseMatcher
                        [ "CAVE.dark"
                        , "*.light.location=PLAYER"
                        ]
                    )
        , test "unsuccessful" <|
            \() ->
                Expect.equal
                    (Err "expecting end at row 1, col 2")
                    (parseMultiple parseMatcher
                        [ "CAVE.dark"
                        , "*zzz.light.location=PLAYER"
                        ]
                    )
        ]


parseRuleTest =
    describe "rule parsing"
        [ test "full example" <|
            \() ->
                Expect.equal
                    ({ trigger = EntityTrigger <| MatchAny [ HasTag "line" ]
                     , conditions =
                        [ Match "PLAYER" [ HasStat "chapter" EQ <| SpecificStat 1 ]
                        , Match "BROADWAY_STREET" [ HasStat "leaving_broadway_street_station_plot" EQ <| SpecificStat 1 ]
                        ]
                     , changes =
                        [ Update "BRIEFCASE" [ SetLink "location" <| SpecificLinkTarget "THIEF" ]
                        , Update "BROADWAY_STREET" [ SetStat "leaving_broadway_street_station_plot" 2 ]
                        ]
                     }
                        |> Ok
                    )
                    (parseRule (always identity)
                        ( """
                          ON: *.line

                          IF: PLAYER.chapter=1
                              BROADWAY_STREET.leaving_broadway_street_station_plot=1

                          DO: BRIEFCASE.location=THIEF
                              BROADWAY_STREET.leaving_broadway_street_station_plot=2
                          """
                        , {}
                        )
                    )
        , test "with minimal spaces" <|
            \() ->
                Expect.equal
                    ({ trigger = EntityTrigger <| MatchAny [ HasTag "line" ]
                     , conditions = [ Match "PLAYER" [ HasStat "chapter" EQ <| SpecificStat 1 ] ]
                     , changes = [ Update "BRIEFCASE" [ SetLink "location" <| SpecificLinkTarget "THIEF" ] ]
                     }
                        |> Ok
                    )
                    (parseRule (always identity)
                        ( """ON: *.line
                             IF: PLAYER.chapter=1
                             DO: BRIEFCASE.location=THIEF"""
                        , {}
                        )
                    )
        , test "no do" <|
            \() ->
                Expect.equal
                    ({ trigger = EntityTrigger <| MatchAny [ HasTag "line" ]
                     , conditions = [ Match "PLAYER" [ HasStat "chapter" EQ <| SpecificStat 1 ] ]
                     , changes = []
                     }
                        |> Ok
                    )
                    (parseRule (always identity)
                        ( """ON: *.line
                             IF: PLAYER.chapter=1
                          """
                        , {}
                        )
                    )
        , test "no if" <|
            \() ->
                Expect.equal
                    ({ trigger = EntityTrigger <| MatchAny [ HasTag "line" ]
                     , conditions = []
                     , changes = [ Update "BRIEFCASE" [ SetLink "location" <| SpecificLinkTarget "THIEF" ] ]
                     }
                        |> Ok
                    )
                    (parseRule (always identity)
                        ( """ON: *.line
                             DO: BRIEFCASE.location=THIEF"""
                        , {}
                        )
                    )
        , test "no do and no if" <|
            \() ->
                Expect.equal
                    ({ trigger = EntityTrigger <| MatchAny [ HasTag "line" ]
                     , conditions = []
                     , changes = []
                     }
                        |> Ok
                    )
                    (parseRule (always identity)
                        ( "ON: *.line"
                        , {}
                        )
                    )
        ]


parseRulesTest =
    test "parseRules" <|
        \() ->
            Expect.equal
                (Ok <|
                    Dict.fromList
                        [ ( "one"
                          , { trigger = EntityTrigger <| Match "PLAYER" []
                            , conditions = []
                            , changes = []
                            , extra = True
                            }
                          )
                        , ( "two"
                          , { trigger = EntityTrigger <| Match "CAVE" []
                            , conditions = []
                            , changes = []
                            , extra = False
                            }
                          )
                        ]
                )
                (parseRules
                    (\{ extra } rule ->
                        { trigger = rule.trigger
                        , conditions = rule.conditions
                        , changes = rule.changes
                        , extra = extra
                        }
                    )
                 <|
                    Dict.fromList
                        [ ( "one", ( "ON: PLAYER", { extra = True } ) )
                        , ( "two", ( "ON: CAVE", { extra = False } ) )
                        ]
                )


shouldFail message res =
    case res of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail message



{-

   ## QUERY exmaples (lists of matching entities)

   // Get a list of all of the locations:

       *.location

   // Get all items in the player's inventory:

       *.item.location=PLAYER

   // Test if the player has any item with enough illumination (if matches is not empty):

       *.item.location=PLAYER.illumination>5

   // Test if any characters in the cave are afraid (if matches is not empty):

       *.character.location=CAVE.fear>5

   // Test if the player is in the cave and afraid (either an empty query results, or the player entity)

       PLAYER.location=CAVE.fear>5

   RULES examples

      trigger: CAVE.!explored
      conditions:
      *.item.location=PLAYER.illumination>5
      changes:
      PLAYER.location=CAVE.fear+2
      CAVE.explored
      narrative: You can see a short ways into the cave, and bravely enter.  You hear an awful snoring sound...


      trigger: GOBLIN.sleeping
      changes:
      GOBLIN.-sleeping
      PLAYER.fear=9
      narrative: There's an old saying, "Let sleeping dogs lie."  That applies double when it comes to goblins.  Too late...

      // trigger match in conditional
      trigger: *.location
      conditions: *.enemy.location=$
      narrative: The {$.name} is too dangerous to enter now...
      // note, there is no way to reference the name/description of the enemy matcher


      // moving around
      trigger: *.location
      changes: PLAYER.location=$

      // picking stuff up
      trigger: *.item.!location=PLAYER
      changes: $.location=PLAYER
      narrative: This might be useful.

      nested (PLAYER.location=(*.dark))
-}
