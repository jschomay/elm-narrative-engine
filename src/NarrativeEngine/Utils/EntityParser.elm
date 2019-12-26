module NarrativeEngine.Utils.EntityParser exposing (ParsedEntity, idParser, numberParser, parseEntity, parseMany, propertyNameParser)

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

import Dict
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.Helpers as Helpers exposing (..)
import Parser exposing (..)


type alias ParsedEntity a =
    Result String ( ID, NarrativeComponent a )


type alias ParsedWorldModel a =
    Result ParseErrors (WorldModel a)


{-| A function for "merging" extra fields into a `NarrativeComponent {}`.
-}
type alias ExtendFn a =
    a -> NarrativeComponent {} -> NarrativeComponent a


{-| Parses a list of entities into a world model. The list of entities are tuples of the entity syntax for parsing, and the extra fields for that entity. You also need to provide an "extend function" to "merge" extra fields with the standard entity fields.
-}
parseMany : ExtendFn a -> List ( String, a ) -> ParsedWorldModel a
parseMany extendFn entities =
    let
        displayError source e =
            ( "Entity def: " ++ source, e )

        addParsedEntity (( source, extraFields ) as entity) acc =
            case parseEntity extendFn entity of
                Ok ( id, parsedEntity ) ->
                    Result.map (Dict.insert id parsedEntity) acc

                Err err ->
                    case acc of
                        Ok _ ->
                            Err [ displayError source err ]

                        Err errors ->
                            Err <| displayError source err :: errors
    in
    List.foldl addParsedEntity (Ok Dict.empty) entities


{-| Parses a single entity, represented as a string of entity syntax and a record of
additional fields. The extend function is used to "merge" the additional fields into the standard entity record. (You can use `always identity` if you don't have any extra fields).
-}
parseEntity : ExtendFn a -> ( String, a ) -> ParsedEntity a
parseEntity extendFn ( text, extraFields ) =
    run entityParser text
        |> Result.map (Tuple.mapSecond <| extendFn extraFields)
        |> Result.mapError Helpers.deadEndsToString


entityParser : Parser ( ID, NarrativeComponent {} )
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
