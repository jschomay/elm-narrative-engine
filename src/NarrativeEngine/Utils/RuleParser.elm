module NarrativeEngine.Utils.RuleParser exposing (ParsedChanges, ParsedEntity, ParsedMatcher, parseChanges, parseEntity, parseMatcher)

import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.Helpers as Helpers exposing (..)
import Parser exposing (..)


type alias ParsedEntity =
    Result String ( ID, NarrativeComponent {} )


type alias ParsedMatcher =
    Result String EntityMatcher


type alias ParsedChanges =
    Result String ChangeWorld


parseEntity : String -> ParsedEntity
parseEntity text =
    run entityParser text
        |> Result.mapError Helpers.deadEndsToString


parseMatcher : String -> ParsedMatcher
parseMatcher text =
    run (matcherParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


parseChanges : String -> ParsedChanges
parseChanges text =
    run (changesParser |. end) text
        |> Result.mapError Helpers.deadEndsToString


entityParser =
    let
        toEntity id narrativeComponent =
            ( id, narrativeComponent )
    in
    succeed toEntity
        |= idParser
        |= propsParser
        |. end


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


{-| IDs must start with a letter, then optionally have more letters, digits, or
special characters.
-}
idParser : Parser ID
idParser =
    let
        valid c =
            Char.isAlphaNum c || List.member c [ '_', '-', ':', '#', '+' ]
    in
    succeed ()
        |. chompIf Char.isAlpha
        |. chompWhile valid
        |> getChompedString
        |> andThen notEmpty


propsParser : Parser (NarrativeComponent {})
propsParser =
    let
        emptyNarrativeComponent =
            { tags = emptyTags
            , stats = emptyStats
            , links = emptyLinks
            }

        toComponent key fn =
            fn key

        helper acc =
            oneOf
                [ succeed toComponent
                    |. spaces
                    |. symbol "."
                    |= propertyNameParser
                    |= oneOf
                        [ succeed identity
                            |. symbol "="
                            |= oneOf
                                [ idParser |> map (\v -> \k -> Loop <| setLink k v acc)
                                , numberParser |> map (\v -> \k -> Loop <| setStat k v acc)
                                ]
                        , succeed (\t -> Loop <| addTag t acc)
                        ]
                , succeed (Done acc)
                ]
    in
    loop emptyNarrativeComponent helper


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


{-| Can't use `int` because a "." can follow the number ("X.a.b=1.c"), and `int`
doesn't allow a digit followed by a ".". This also handles negatives.
-}
numberParser : Parser Int
numberParser =
    let
        int_ =
            chompWhile Char.isDigit
                |> getChompedString
                |> andThen
                    (String.toInt
                        >> Maybe.map succeed
                        >> Maybe.withDefault (problem "not an int")
                    )
    in
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int_
        , int_
        ]


propertyNameParser : Parser String
propertyNameParser =
    let
        valid c =
            Char.isAlphaNum c || List.member c [ '_', ':', '#' ]
    in
    succeed ()
        |. chompWhile valid
        |> getChompedString
        |> andThen notEmpty
