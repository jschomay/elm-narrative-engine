module NarrativeEngine.Utils.EntityParser exposing (ParsedEntity, idParser, numberParser, parseEntity, propertyNameParser)

{-| A helper module for easily creating entities.

Syntax is `<entity id>.<props>`, given:

  - Entity ids must start with a letter, then optionally have more letters, digits, or any of '\_', '-', ':', '#' or '+'.
  - Props are separated by periods. Prop keys are alphanumeric and can include '\_', ':' and '#'. Tags are specified just as a key. Stats and links have a key then an '=' then an int or entity id respectively.

Example:

    CAVE_ENTRANCE.location.light_level=3
    PLAYER.location=CAVE_ENTRANCE

This makes an entity with id "CAVE\_ENTRANCE" that has a tag of "location" and a stat of "light\_level" with a value of 3. And another entity of id "PLAYER" with a link of "location" with a value of "CAVE\_ENTRANCE".

(By convention, entity ids are capitalized and prop keys are snake case.)

Note that `RuleParser` relies on some of the parsers in this module.

-}

import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.Helpers as Helpers exposing (..)
import Parser exposing (..)


type alias ParsedEntity =
    Result String ( ID, NarrativeComponent {} )


parseEntity : String -> ParsedEntity
parseEntity text =
    run entityParser text
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


{-| IDs must start with a letter, then optionally have more letters, digits, or special characters.
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


{-| A parser that makes `NarrativeComponent`s.
-}
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


{-| A parser for valid property names
-}
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
