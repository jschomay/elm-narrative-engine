module Utils.NarrativeParserTest exposing (all)

import Dict
import Expect
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.NarrativeParser exposing (parse, parseMany)
import Result
import Test exposing (..)


all =
    describe "parsing narrative"
        [ static
        , cycle
        , property
        , mixed
        , withContinues
        , conditional
        , parseManyTest
        ]


config =
    { cycleIndex = 0
    , propKeywords = Dict.empty
    , worldModel = Dict.empty
    , trigger = ""
    }


configWithKeyword k f =
    { config
        | propKeywords = Dict.fromList [ ( k, f ) ]
    }


configWithName =
    configWithKeyword "name"
        (\s ->
            case s of
                "MrX" ->
                    Ok "Mr. X"

                _ ->
                    Err <| "Can't find " ++ ".name" ++ " for " ++ s
        )


entity id =
    ( id
    , { tags = emptyTags
      , stats = emptyStats
      , links = emptyLinks
      }
    )


configWithNameError =
    configWithKeyword "name" (always <| Err "test error")


configWithWorldModel =
    { configWithName
        | worldModel =
            Dict.fromList
                [ entity "MrX"
                    |> tag "known"
                    |> tag "character"
                , entity "Player"
                    |> tag "character"
                    |> link "location" "House"
                , entity "House"
                    |> tag "location"
                ]
    }


static =
    describe "static"
        [ test "empty string" <|
            \() ->
                -- TODO this was an empty string in a list, but it should be an empty
                -- list
                Expect.equal [] <|
                    parse { config | cycleIndex = 0 } ""
        , test "static string" <|
            \() ->
                Expect.equal [ "just a string" ] <|
                    parse { config | cycleIndex = 0 } "just a string"
        , test "erors with orphan closer" <|
            \() ->
                shouldError "a}b should error" <| parse { config | cycleIndex = 0 } "a}b"
        , test "erors with orphan opener" <|
            \() ->
                shouldError "a{b should error" <| parse { config | cycleIndex = 0 } "a{b"
        ]


cycle =
    describe "cycles"
        [ test "cycle at 0" <|
            \() ->
                Expect.equal [ "a" ] <|
                    parse { config | cycleIndex = 0 } "{a|b|c}"
        , test "cycle at 2" <|
            \() ->
                Expect.equal [ "c" ] <|
                    parse { config | cycleIndex = 2 } "{a|b|c}"
        , test "cycle out of bounds" <|
            \() ->
                Expect.equal [ "c" ] <|
                    parse { config | cycleIndex = 9 } "{a|b|c}"
        , test "cycle with empties 1" <|
            \() ->
                Expect.equal [] <|
                    parse { config | cycleIndex = 1 } "{||ok|no}"
        , test "cycle with empties 2" <|
            \() ->
                Expect.equal [ "ok" ] <|
                    parse { config | cycleIndex = 2 } "{||ok|no}"
        , test "empty cycle" <|
            \() ->
                -- NOTE maybe this should be an error?
                Expect.equal [ "ab" ] <|
                    parse { config | cycleIndex = 2 } "a{}b"
        , test "two cycles" <|
            \() ->
                Expect.equal [ "bb" ] <|
                    parse { config | cycleIndex = 1 } "{a|b|c}{a|b|c}"
        , test "cycle in middle" <|
            \() ->
                Expect.equal [ "hello good bye" ] <|
                    parse { config | cycleIndex = 2 } "hello {world|good} bye"
        , test "cycles on ends" <|
            \() ->
                Expect.equal [ "two three five" ] <|
                    parse { config | cycleIndex = 2 } "{one|two} three {four|five}"
        , describe "cycles that looks similar to a prop (but isn't)" <|
            let
                text =
                    -- note, a keyword ` X|Henry` of would actually match
                    "Meet {Mr. X|Henry}.{  He says you can call him Henry.|}"
            in
            [ test "cycle 0" <|
                \() ->
                    Expect.equal [ "Meet Mr. X.  He says you can call him Henry." ] <|
                        parse config text
            , test "cycle 1" <|
                \() ->
                    Expect.equal [ "Meet Henry." ] <|
                        parse { config | cycleIndex = 1 } text
            ]
        , test "erors with orphan opener in cycle" <|
            \() ->
                shouldError "{abc{xyz} should error" <|
                    parse { config | cycleIndex = 0 } "{abc{xyz}"
        , test "erors with orphan closer outside cycle" <|
            \() ->
                shouldError "{abc}xyz} should error" <|
                    parse { config | cycleIndex = 0 } "{abc}xyz}"
        , test "looping cycles" <|
            \() ->
                Expect.equal [ [ "Heads" ], [ "Tails" ], [ "Heads" ], [ "Tails" ] ] <|
                    List.map (\i -> parse { config | cycleIndex = i } "{~Heads|Tails}") <|
                        List.range 0 3
        , test "random cycles" <|
            \() ->
                Expect.equal [ [ "Heads" ], [ "Heads" ], [ "Tails" ], [ "Tails" ] ] <|
                    List.map (\i -> parse { config | trigger = "COIN", cycleIndex = i } "{?Heads|Tails}") <|
                        List.range 0 3
        ]


property =
    describe "property"
        [ test "happy path" <|
            \() ->
                Expect.equal [ "Meet Mr. X." ] <|
                    parse configWithName "Meet {MrX.name}."
        , test "looks like prop but really cycle" <|
            \() ->
                Expect.equal [ "MrX.name" ] <|
                    parse configWithName "{MrX.name|hi}"
        , test "if the keyword function returns an Err it doesn't match (will match as a cycle instead)" <|
            \() ->
                Expect.equal [ "MrX.name" ] <|
                    parse configWithNameError "{MrX.name}"
        , test "trigger ('$.name')" <|
            \() ->
                Expect.equal [ "Meet Mr. X" ] <|
                    parse { configWithName | trigger = "MrX" } "Meet {$.name}"
        ]


mixed =
    describe "mixed"
        [ test "nested cycles (works, but no real use case)" <|
            \() ->
                Expect.equal [ "onetwo" ] <|
                    parse { config | cycleIndex = 0 } "{one{two|three}|four}"
        , test "nested cycles 2 (works, but no real use case)" <|
            \() ->
                Expect.equal [ "four" ] <|
                    parse { config | cycleIndex = 1 } "{one{two|three}|four}"
        , describe "basic prop and cycle mix" <|
            [ test "cycle 0, prop 1" <|
                \() ->
                    Expect.equal [ "a" ] <|
                        parse configWithName "{a|{MrX.name}}"
            , test "cycle 1, prop 1" <|
                \() ->
                    Expect.equal [ "Mr. X" ] <|
                        parse { configWithName | cycleIndex = 1 } "{a|{MrX.name}}"
            , test "cycle 0, prop 0" <|
                \() ->
                    Expect.equal [ "Mr. X" ] <|
                        parse configWithName "{{MrX.name}|a}"
            , test "cycle 1, prop 0" <|
                \() ->
                    Expect.equal [ "a" ] <|
                        parse { configWithName | cycleIndex = 1 } "{{MrX.name}|a}"
            ]
        , describe "complete cycle and props example" <|
            let
                text =
                    "{Meet {MrX.name}|{MrX.nickname} says hi}.{  He says you can call him {MrX.nickname}.|}"

                nestedConfig =
                    { config
                        | propKeywords =
                            Dict.fromList
                                [ ( "name", always <| Ok "Mr. X" )
                                , ( "nickname", always <| Ok "Henry" )
                                ]
                    }
            in
            [ test "cycle 0" <|
                \() ->
                    Expect.equal [ "Meet Mr. X.  He says you can call him Henry." ] <|
                        parse nestedConfig text
            , test "cycle 1" <|
                \() ->
                    Expect.equal [ "Henry says hi." ] <|
                        parse { nestedConfig | cycleIndex = 1 } text
            ]
        , test "nested cycle with conditional" <|
            \() ->
                Expect.equal [ "ayesb" ] <|
                    parse { configWithWorldModel | cycleIndex = 1 } "a{one|{MrX.known?yes|no}}b"
        ]


withContinues =
    describe "continues"
        [ test "basic" <|
            \() ->
                Expect.equal [ "one", "two", "three" ] <|
                    parse { config | cycleIndex = 0 } "one---two---three"
        , test "in cycle 1" <|
            \() ->
                Expect.equal [ "one", "two" ] <|
                    parse { config | cycleIndex = 0 } "{one---two|three}"
        , test "in cycle 2" <|
            \() ->
                Expect.equal [ "three" ] <|
                    parse { config | cycleIndex = 1 } "{one---two|three}"
        ]


conditional =
    describe "conditional" <|
        [ test "if (true)" <|
            \() ->
                Expect.equal [ "A man walks by.  You recognize him as Mr. X." ] <|
                    parse configWithWorldModel "A man walks by.{MrX.known?  You recognize him as Mr. X.}"
        , test "if (false)" <|
            \() ->
                Expect.equal [ "A man walks by." ] <|
                    parse configWithWorldModel "A man walks by.{MrX.!known? You don't recognize him.}"
        , test "malformed query (get's parsed as a cycle)" <|
            \() ->
                Expect.equal [ "Entity.badstat>x? yes " ] <|
                    parse configWithWorldModel "{Entity.badstat>x? yes | no}"
        , test "if/else (else branch)" <|
            \() ->
                Expect.equal [ "A man walks by.  You recognize him as Mr. X." ] <|
                    parse configWithWorldModel "A man walks by.  You {MrX.!known? don't recognize him|recognize him as Mr. X}."
        , test "with nested query" <|
            \() ->
                Expect.equal [ "yes" ] <|
                    parse configWithWorldModel "{Player.location=(House.location)?yes|no}"
        , test "multiple conditions" <|
            \() ->
                Expect.equal [ "You are alone." ] <|
                    parse configWithWorldModel "{MrX.known.location=House? You see Mr. X in your home.|You are alone.}"
        , test "with whitespace" <|
            \() ->
                -- NOTE the spaces after ? and around | are part of the text
                -- I could eat them, but then it wouldn't be possible to
                -- conditionally all a new paragraph or spaces
                Expect.equal [ " yes" ] <|
                    parse configWithWorldModel """{ MrX.known ? yes|no}"""
        , test "multiple queries (true)" <|
            \() ->
                Expect.equal [ "A man rings the bell.  You recognize him as Mr. X." ] <|
                    parse configWithWorldModel "A man rings the bell.{MrX.known & Player.location=House ?  You recognize him as Mr. X.|  But you are not home.}"
        , test "multiple queries (false)" <|
            \() ->
                Expect.equal [ "A man rings the bell.  But you are not home." ] <|
                    parse configWithWorldModel "A man rings the bell.{MrX.known & Player.location=Other ?  You recognize him as Mr. X.|  But you are not home.}"
        , test "with * (true)" <|
            \() ->
                Expect.equal [ "Someone is home." ] <|
                    parse configWithWorldModel "{*.character.location=House?Someone|No one} is home."
        , test "with * (false)" <|
            \() ->
                Expect.equal [ "No one is home." ] <|
                    parse configWithWorldModel "{*.character.location=Other?Someone|No one} is home."
        , test "matching trigger (in match)" <|
            \() ->
                Expect.equal [ "Home sweet home." ] <|
                    parse { configWithWorldModel | trigger = "House" } "{Player.location=$ ?Home sweet home.|You miss your home.}"
        , test "matching trigger (in match, nested)" <|
            \() ->
                Expect.equal [ "Home sweet home." ] <|
                    parse { configWithWorldModel | trigger = "House" } "{Player.location=($.location) ?Home sweet home.|You miss your home.}"
        , test "matching trigger (in selector)" <|
            \() ->
                Expect.equal [ "You know him." ] <|
                    parse { configWithWorldModel | trigger = "MrX" } "{$.known?You know him.|You don't know him.}"
        , test "with nested cycle" <|
            \() ->
                Expect.equal [ "You are still home." ] <|
                    parse { configWithWorldModel | cycleIndex = 1 } "{Player.location=House?You are {|still} home.}"
        ]


allTogether =
    describe "all together" <|
        let
            text =
                """You shout at {$.name}{| again}.
"{What do you want?|Why do you keep bothering me?|Leave me alone!}"
{$.suspicious>3 & Warrant.location=Player?"You're under arrest!"|"Never mind."}
"""
        in
        [ test "cycle 1" <|
            \() ->
                Expect.equal
                    [ """You shout at Mr. X.
"What do you want?"
"Never mind."
"""
                    ]
                <|
                    parse { configWithName | trigger = "MrX" } text
        , test "cycle 2" <|
            \() ->
                Expect.equal
                    [ """You shout at Mr. X again.
"Why do you keep bothering me?"
"You're under arrest!"
""" ]
                <|
                    parse
                        { configWithName
                            | trigger = "MrX"
                            , cycleIndex = 1
                            , worldModel =
                                Dict.fromList
                                    [ entity "MrX"
                                        |> stat "suspicious" 5
                                    , entity "Warrant"
                                        |> link "location" "Player"
                                    , entity "Player"
                                    ]
                        }
                        text
        ]


parseManyTest =
    describe "parseMany"
        [ test "ok" <|
            \() ->
                Expect.equal (Ok ()) <|
                    parseMany <|
                        Dict.fromList [ ( "first", "This {is| is just} fine." ) ]
        , test "errors" <|
            \() ->
                Expect.equal
                    (Err
                        [ ( "Narrative content: bad\n{{no good} "
                          , "expecting '?' at row 1, col 11; problem cannot be empty at row 1, col 2; expecting symbol '|' at row 1, col 11; expecting symbol '}' at row 1, col 11"
                          )
                        , ( "Narrative content: also bad\n{{also no good} "
                          , "expecting '?' at row 1, col 16; problem cannot be empty at row 1, col 2; expecting symbol '|' at row 1, col 16; expecting symbol '}' at row 1, col 16"
                          )
                        ]
                    )
                <|
                    parseMany <|
                        Dict.fromList
                            [ ( "ok", "This {is| is just} fine." )
                            , ( "bad", "{{no good}" )
                            , ( "also bad", "{{also no good}" )
                            ]
        ]


shouldError message res =
    Expect.true message <| List.all identity <| List.map (String.startsWith "ERROR") res
