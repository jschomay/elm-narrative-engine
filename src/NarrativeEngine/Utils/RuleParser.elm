module NarrativeEngine.Utils.RuleParser exposing
    ( ParsedMatcher, parseMatcher
    , ParsedChanges, parseChanges
    , StringRule, ExtendFn, ParsedRules, ParsedRule, parseRule, parseRules
    )

{-| A helper module for easily authoring entity matchers and world changes for rules and queries.

Example:

```text
Entity matchers:
PLAYER.current_location=(*.dark).fear<5
CAVE.dark.!explored
*.enemy.current_location=CAVE

Change world:
PLAYER.current_location=$.fear+1
CAVE.explored
(*.enemy).blinded
```

Tags: `.tag1.tag2.tag2`

Stats: `.stat1=1.stat2>0.stat3<-2`
Showing equal, greater than and less than for positive and negative integers.

Also you can do a comparison against another entity stats like: `.stat_4>(stat ID.other_stat).stat_5<(stat ID.other_stat).stat_6=(stat ID.other_stat)`

Links: `.link1=ID.link2=(*.tag1)`
Showing a direct link and a generic link with nested queries.

Also you can do a comparison against another entity links like: `.link3=(link ID.other_link)`

Any query segment can be prefixed with a `!` for not: `.!tag1.!stat>9.!link=ID`

@docs ParsedMatcher, parseMatcher

@docs ParsedChanges, parseChanges

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


{-| Takes a dictionary of rules defined with "entity matcher" and "change world" syntax and parses into `NarrativeEngine.Core.Rules.Rules`.

You must include a function for extended fields (see `parseRule` for more details), which will be called with each rule that gets parsed.

-}
parseRules : ExtendFn a -> Dict RuleID (StringRule a) -> ParsedRules a
parseRules extendFn rules =
    let
        printRule { trigger, conditions, changes } =
            "Trigger: "
                ++ trigger
                ++ "\nConditions: "
                ++ String.join ", " conditions
                ++ "\nChanges: "
                ++ String.join ", " changes

        displayError k v e =
            ( "Rule: " ++ k ++ "\n" ++ printRule v ++ " ", e )

        addParsedRule id stringRule acc =
            case parseRule extendFn stringRule of
                Ok parsedRule ->
                    Result.map (Dict.insert id parsedRule) acc

                Err err ->
                    case acc of
                        Ok _ ->
                            Err [ displayError id stringRule err ]

                        Err errors ->
                            Err <| displayError id stringRule err :: errors
    in
    Dict.foldl addParsedRule (Ok Dict.empty) rules


{-| Parse a rule defined with "entity matcher" and "change world" syntax into a `NarrativeEngine.Core.Rules.Rule`.

Since rules are extensible records, you must supply a function that extends the base rule. For example, if you include a "soundEffect" field on your rule, you can build a new rule record from the parsed rule and your original rule. If you do not use extra fields, just pass `always identity`.

-}
parseRule : ExtendFn a -> StringRule a -> ParsedRule a
parseRule extendFn ({ trigger, conditions, changes } as initialRule) =
    let
        toRule trigger_ conditions_ changes_ =
            { trigger = trigger_
            , conditions = conditions_
            , changes = changes_
            }
                |> extendFn initialRule
    in
    Result.map3 toRule
        (parseMatcher trigger)
        (parseMultiple parseMatcher conditions)
        (parseMultiple parseChanges changes)


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
                |= idParser
                |. symbol "."
                |= propertyNameParser
                |. symbol ")"

        helper acc =
            oneOf
                [ succeed (toQuery acc)
                    |. chompWhile ((==) ' ')
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
