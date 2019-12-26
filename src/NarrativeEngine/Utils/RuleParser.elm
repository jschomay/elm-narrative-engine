module NarrativeEngine.Utils.RuleParser exposing (ParsedChanges, ParsedMatcher, ParsedRule, ParsedRules, parseChanges, parseMatcher, parseRule, parseRules)

{-| A helper module for parsing queries and rules.

Example ... TODO

-}

import Dict exposing (Dict)
import NarrativeEngine.Core.Rules exposing (..)
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.EntityParser exposing (idParser, numberParser, propertyNameParser)
import NarrativeEngine.Utils.Helpers as Helpers exposing (..)
import Parser exposing (..)


type alias ParsedRules a =
    Result ParseErrors (Rules a)


type alias ParsedRule a =
    Result String (Rule a)


type alias ParsedMatcher =
    Result String EntityMatcher


type alias ParsedChanges =
    Result String ChangeWorld


{-| The rule shape, but with Strings of matcher and changes syntax.
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


{-| Takes a dictionary of rules defined with matcher and changes syntax and parses into complete rules.

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


{-| Parses a rule defined with "matcher" and "changes" syntax into a complete rule.

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


parseMatcher : String -> ParsedMatcher
parseMatcher text =
    run (matcherParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


parseChanges : String -> ParsedChanges
parseChanges text =
    run (changesParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


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


{-| A valid id, or "\*" for `MatchAny` or "$" to indicate the id should be replaced
with a "trigger" (useful in conditional narratives for example).
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


queriesParser : Parser (List Query)
queriesParser =
    let
        toQuery acc negate propName queryConstructor =
            if negate then
                Loop <| (Not <| queryConstructor propName) :: acc

            else
                Loop <| queryConstructor propName :: acc

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
                            |= (numberParser |> map (\n -> \key -> HasStat key GT n))
                        , succeed identity
                            |. symbol "<"
                            |= (numberParser |> map (\n -> \key -> HasStat key LT n))
                        , succeed identity
                            |. symbol "="
                            |= oneOf
                                [ numberParser |> map (\n -> \key -> HasStat key EQ n)
                                , symbol "$" |> map (\_ -> \key -> HasLink key (Match "$" []))
                                , idParser |> map (\id -> \key -> HasLink key (Match id []))
                                , succeed identity
                                    |. symbol "("
                                    |= (matcherParser |> map (\matcher -> \key -> HasLink key matcher))
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
                                        , symbol "$" |> map (\_ -> \key -> SetLink key "$")
                                        , idParser |> map (\id -> \key -> SetLink key id)
                                        ]
                                , succeed (\t -> AddTag t)
                                ]
                        ]
                , succeed (Done acc)
                ]
    in
    loop [] helper
