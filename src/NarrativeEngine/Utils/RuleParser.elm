module NarrativeEngine.Utils.RuleParser exposing
    ( ParsedMatcher, parseMatcher
    , ParsedChanges, parseChanges
    , StringRule, ExtendFn, ParsedRules, ParsedRule, parseRule, parseRules
    )

{-| A helper module for easily authoring "entity matchers" and "world changes" for rules and queries.


## Entity matchers / queries

Example syntax:

```text
PLAYER.current_location=(*.dark).fear<5
CAVE.dark.!explored
*.enemy.current_location=CAVE
```

These all will get parsed into `NarrativeEngine.WorldModel.EntityMatcher`s.

The format is `<entity ID or * or $><one or more queries as defined below>`

`*` will become a `MatchAny`, otherwise it will be a `Match`. `$` is passed through directly (to be replaced with the trigger ID eventually).

Each query starts with a `.`. Query details follow.

Any query segment can be prefixed with a `!` for not: `ID.!tag1.!stat>9.!link=ID2`


### Tags

`.tag1.tag2.tag2`

Each tag becomes a `HasTag` with the value provided.


### Stats

Specific: `.stat1=1.stat2>0.stat3<-2`

Each stat becomes a `HasStat` with the key and value provided using `SpecificStat`. You can use equals, greater than and less than, and positive and negative integers.

Compare: `.stat_4>(stat ID.other_stat).stat_5<(stat ID.other_stat).stat_6=(stat ID.other_stat)`

Each stat becomes a `HasStat` with the key provided and uses `CompareStat` with the ID and stat key to compare against supplied in the parens. You can use equals, greater than and less than.


### Links

Specific: `.link1=ID1.link2=(ID2.tag1).link3=(*.tag2)`

Becomes a `HasLink` with the specified key and value as a `SpecificLink`. If you use parens you can add nested queries that apply to the target entity. You can also use `*` to `MatchAny` in nested queries.

Compare: `.link4=(link ID3.other_link)`

Becomes a `HasLink` with the specified key and a value of `CompareLink` with the id and link key supplied in the parens.

@docs ParsedMatcher, parseMatcher


## Changes syntax

Example syntax:

```text
Change world:
PLAYER.current_location=$.fear+1
CAVE.explored
(*.enemy).blinded
```

These all will get parsed into `NarrativeEngine.WorldModel.ChangeWorld`s.

The format is `<entity ID or * or $><one or more changes as defined below>`

To `UpdateAll`, use a generic matcher in parens. Otherwise use an ID for a specific `Update`. `$` is passed through directly (to be replaced with the trigger ID eventually).

Each change starts with a `.`.

Tags: `ID.tag1.-tag2` - add "tag1" remove "tag2"

Stats: `ID.stat1=1.stat2+1.stat3-2` - set "stat1" to 1, increment "stat2" by 1 and decrement "stat3" by 2.

Links: `ID.link1=ID2.link2=(link ID2.link1)` - set "link1" to "ID2", set "link2" to whatever "ID2" has for "link1" (No-op if the target id or link doesn't exit).

@docs ParsedChanges, parseChanges


## Rules syntax

      ON: *.line

      IF: PLAYER.chapter=1
          BROADWAY_STREET.leaving_broadway_street_station_plot=1

      DO: BRIEFCASE.location=THIEF
          BROADWAY_STREET.leaving_broadway_street_station_plot=2

You can include spaces and newlines as desired. The `:` after each rule part is optional. You can also leave out the "IF" and/or "DO" parts.

In general you should use `parseRules` at the top level of you application, and display any errors with `NarrativeEngine.Utils.Helpers.parseErrorsView`.

@docs StringRule, ExtendFn, ParsedRules, ParsedRule, parseRule, parseRules

-}

import Dict exposing (Dict)
import NarrativeEngine.Core.Rules exposing (..)
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.EntityParser exposing (idParser, numberParser, propertyNameParser)
import NarrativeEngine.Utils.Helpers as Helpers exposing (..)
import Parser exposing (..)


{-| The result of parsing a collection of `StringRule`s.
-}
type alias ParsedRules a =
    Result ParseErrors (Rules a)


{-| The result of parsing a `StringRule`.
-}
type alias ParsedRule a =
    Result String (Rule a)


{-| The result of parsing an "entity matcher" syntax string.
-}
type alias ParsedMatcher =
    Result String EntityMatcher


{-| The result of parsing a "change world" syntax string.
-}
type alias ParsedChanges =
    Result String ChangeWorld


{-| The rule shape, but with "entity matcher" and "change world" syntax strings.
-}
type alias StringRule a =
    { a
        | trigger : String
        , conditions : List String
        , changes : List String
    }


{-| A function that receives a potentially extended `StringRule a` and a record with the main rule fields, to build a `Rule a`. This is how you "bring over" any extra fields when parsing a rule.
-}
type alias ExtendFn a =
    StringRule a -> Rule {} -> Rule a


type alias ExtendFn_ a =
    a -> Rule {} -> Rule a


{-| Parses a list of "rule" syntax strings. The dict of rules are tuples of the "rule" syntax for parsing, and the extra fields for that rule. You also need to provide an "extend function" to "merge" extra fields with the standard entity fields.
-}
parseRules : ExtendFn_ a -> Dict RuleID ( String, a ) -> ParsedRules a
parseRules extendFn rules =
    let
        displayError k v e =
            ( "Rule: " ++ k ++ "\n" ++ Tuple.first v ++ " ", e )

        addParsedRule id ruleSpec acc =
            case parseRule extendFn ruleSpec of
                Ok parsedRule ->
                    Result.map (Dict.insert id parsedRule) acc

                Err err ->
                    case acc of
                        Ok _ ->
                            Err [ displayError id ruleSpec err ]

                        Err errors ->
                            Err <| displayError id ruleSpec err :: errors
    in
    Dict.foldl addParsedRule (Ok Dict.empty) rules


{-| Parses a single "rule" syntax string along with a record of additional fields. The extend function is used to "merge" the additional fields into the standard rule record. (You can use `always identity` if you don't have any extra fields).
-}
parseRule : ExtendFn_ a -> ( String, a ) -> ParsedRule a
parseRule extendFn ( source, extraFields ) =
    run (ruleParser |. end) source
        |> Result.map (extendFn extraFields)
        |> Result.mapError Helpers.deadEndsToString


{-| Parses something like:

      ON: *.line

      IF: PLAYER.chapter=1
          BROADWAY_STREET.leaving_broadway_street_station_plot=1

      DO: BRIEFCASE.location=THIEF
          BROADWAY_STREET.leaving_broadway_street_station_plot=2

You can include spaces and newlines as desired. The `:` after each rule part is optional. You can also leave out the "IF" and/or "DO" parts.

-}
ruleParser : Parser (Rule {})
ruleParser =
    let
        toRule trigger conditions changes =
            { trigger = trigger
            , conditions = conditions
            , changes = changes
            }

        triggerParser =
            succeed identity
                |. spaces
                |. oneOf [ keyword "ON:", keyword "ON" ]
                |. spaces
                |= matcherParser
                |. spaces

        -- Note, this chomps the "DO(:) " if it finds it, so don't look for that after
        -- this parser
        conditionsParser =
            succeed identity
                -- chomps the "IF" if it is there, but if not, it will hit the "DO"
                -- or end with `[]` before doing any entity matchers, so it can
                -- ignore "IF" too
                |. oneOf [ keyword "IF:", keyword "IF", succeed () ]
                |. spaces
                |= loop []
                    (\acc ->
                        oneOf
                            [ oneOf [ keyword "DO:", keyword "DO", end ]
                                |. spaces
                                |> map (\_ -> Done <| List.reverse acc)
                            , matcherParser
                                |. spaces
                                |> map (\condition -> Loop <| condition :: acc)
                            ]
                    )

        changesParser_ =
            loop []
                (\acc ->
                    oneOf
                        [ end |> map (\_ -> Done <| List.reverse acc)
                        , changesParser
                            |. spaces
                            |> map (\condition -> Loop <| condition :: acc)
                        ]
                )
    in
    succeed toRule
        |= triggerParser
        |= conditionsParser
        |= changesParser_


{-| Parse an "entity matcher" syntax string.
-}
parseMatcher : String -> ParsedMatcher
parseMatcher text =
    run (matcherParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


{-| Parse a "change world" syntax string.
-}
parseChanges : String -> ParsedChanges
parseChanges text =
    run (changesParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


{-| Parse an "entity matcher" syntax string.
-}
matcherParser : Parser EntityMatcher
matcherParser =
    let
        toMatcher selector queries =
            selector queries
    in
    succeed toMatcher
        |= selectorParser
        |= queriesParser


changesParser : Parser ChangeWorld
changesParser =
    let
        toChange selector updates =
            selector updates
    in
    succeed toChange
        |= updateTargetParser
        |= changeEntityParser


{-| A valid id, or "\*" for `MatchAny` or "$" to indicate the id should be replaced with a "trigger" (useful in conditional narratives for example).
-}
selectorParser : Parser (List Query -> EntityMatcher)
selectorParser =
    oneOf
        [ symbol "*" |> map (always MatchAny)
        , symbol "$" |> map (always <| Match "$")
        , idParser |> map Match
        ]


updateTargetParser : Parser (List ChangeEntity -> ChangeWorld)
updateTargetParser =
    oneOf
        [ symbol "$" |> map (always <| Update "$")
        , idParser |> map Update
        , succeed identity
            |. symbol "("
            |. symbol "*"
            |= (queriesParser |> map (\queries -> UpdateAll queries))
            |. symbol ")"
        ]


{-| Syntax to parse queries.

Tags: `.tag1.tag2.tag2`

Stats: `.stat1=1.stat2>0.stat3<-2`
Showing equal, greater than and less than for positive and negative integers.

Also you can do a comparison against another entity stats like: `.stat_4>(stat ID.other_stat).stat_5<(stat ID.other_stat).stat_6=(stat ID.other_stat)`

Links: `.link1=ID.link2=(*.tag1)`
Showing a direct link and a generic link with nested queries.

Also you can do a comparison against another entity links like: `.link3=(link ID.other_link)`

Any query segment can be prefixed with a `!` for not: `.!tag1.!stat>9.!link=ID`

-}
queriesParser : Parser (List Query)
queriesParser =
    let
        toQuery acc negate propName queryConstructor =
            if negate then
                Loop <| (Not <| queryConstructor propName) :: acc

            else
                Loop <| queryConstructor propName :: acc

        compareParser kind mapper =
            succeed mapper
                |. keyword ("(" ++ kind)
                |. chompWhile ((==) ' ')
                |= oneOf [ token "$" |> map (always "$"), idParser ]
                |. symbol "."
                |= propertyNameParser
                |. symbol ")"

        helper acc =
            oneOf
                [ succeed (toQuery acc)
                    |. symbol "."
                    |= oneOf
                        [ symbol "!" |> map (always True)
                        , succeed False
                        ]
                    |= propertyNameParser
                    |= oneOf
                        [ succeed identity
                            |. symbol ">"
                            |= oneOf
                                [ compareParser "stat"
                                    (\compareID compareKey key ->
                                        HasStat key GT (CompareStat compareID compareKey)
                                    )
                                , numberParser |> map (\n -> \key -> HasStat key GT (SpecificStat n))
                                ]
                        , succeed identity
                            |. symbol "<"
                            |= oneOf
                                [ compareParser "stat"
                                    (\compareID compareKey key ->
                                        HasStat key LT (CompareStat compareID compareKey)
                                    )
                                , numberParser |> map (\n -> \key -> HasStat key LT (SpecificStat n))
                                ]
                        , succeed identity
                            |. symbol "="
                            |= oneOf
                                [ numberParser |> map (\n -> \key -> HasStat key EQ (SpecificStat n))
                                , symbol "$" |> map (\_ -> \key -> HasLink key (SpecificLink <| Match "$" []))
                                , idParser |> map (\id -> \key -> HasLink key (SpecificLink <| Match id []))
                                , compareParser "stat"
                                    (\compareID compareKey key ->
                                        HasStat key EQ (CompareStat compareID compareKey)
                                    )
                                , compareParser "link"
                                    (\compareID compareKey key ->
                                        HasLink key (CompareLink compareID compareKey)
                                    )

                                -- this needs to come after the compare parsers
                                -- because they also start with "("
                                , succeed identity
                                    |. symbol "("
                                    |= (matcherParser |> map (\matcher -> \key -> HasLink key <| SpecificLink matcher))
                                    |. symbol ")"
                                ]
                        , succeed (\t -> HasTag t)
                        ]
                , succeed (Done acc)
                ]
    in
    loop [] helper


changeEntityParser : Parser (List ChangeEntity)
changeEntityParser =
    let
        toUpdateEntity acc propName updateConstructor =
            Loop <| updateConstructor propName :: acc

        lookupParser mapper =
            succeed mapper
                |. keyword "(link"
                |. chompWhile ((==) ' ')
                |= oneOf [ token "$" |> map (always "$"), idParser ]
                |. symbol "."
                |= propertyNameParser
                |. symbol ")"

        helper acc =
            oneOf
                [ succeed identity
                    |. chompWhile ((==) ' ')
                    |. symbol "."
                    |= oneOf
                        [ succeed (\t -> Loop <| RemoveTag t :: acc)
                            |. symbol "-"
                            |= propertyNameParser
                        , succeed (toUpdateEntity acc)
                            |= propertyNameParser
                            |= oneOf
                                [ succeed identity
                                    |. symbol "+"
                                    |= (numberParser |> map (\n -> \key -> IncStat key n))
                                , succeed identity
                                    |. symbol "-"
                                    |= (numberParser |> map (\n -> \key -> DecStat key n))
                                , succeed identity
                                    |. symbol "="
                                    |= oneOf
                                        [ numberParser |> map (\n -> \key -> SetStat key n)
                                        , symbol "$" |> map (\_ -> \key -> SetLink key <| SpecificLinkTarget "$")
                                        , idParser |> map (\id -> \key -> SetLink key <| SpecificLinkTarget id)
                                        , lookupParser (\lookupID lookupKey key -> SetLink key <| LookUpLinkTarget lookupID lookupKey)
                                        ]
                                , succeed (\t -> AddTag t)
                                ]
                        ]
                , succeed (Done acc)
                ]
    in
    loop [] helper
