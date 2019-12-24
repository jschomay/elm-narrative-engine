module NarrativeEngine.Utils.NarrativeParser exposing (Narrative, parse, parsible)

import Array
import Dict exposing (Dict)
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.Helpers as Helpers exposing (notEmpty, parseMultiple)
import NarrativeEngine.Utils.RuleParser exposing (parseMatcher)
import Parser exposing (..)
import Result


{-| The fully parsed string, split at the "continue" marks (`---`).
-}
type alias Narrative =
    List String


{-| Provides a context for the parser to process narrative correctly. Includes the following keys:

`cycleIndex` - an integer starting at 0 indicating which index of cycle text should be used. Applies to call cycle texts and sticks on the last one. Ex: "{one|two} and {|three}" with a cycleIndex of 1 would produce "two and three"

`propKeywords` - a dictionary of valid keywords to match against, and the corresponding functions that will take an entity ID and return a property as a Result. For example "{stranger.description}" could be matched with a keyword of "description" and a corresponding function that takes "stranger" and returns a description. If it returns and Err, the match will fail.

`worldModel` - the full world model. Used to query against for conditional text.

`trigger` - an entity ID used to replace "$" in conditional text.

-}
type alias Config a =
    { cycleIndex : Int
    , propKeywords : Dict String (String -> Result String String)
    , worldModel : WorldModel a
    , trigger : ID
    }


{-| Parses the text, then splits for continues. Removes empty strings.

Unlike when parsing rules or entities ahead of time, you should call this each time you have a new narrative text to parse. This is because the parsing depends on the `Config`'s current state.

This will always return a string. In the case of a parsing error, it will say "Error". For this reason, you should call `parsible` upon your program init to ensure you have no parse errors from the start.

-}
parse : Config a -> String -> Narrative
parse config text =
    let
        parser =
            -- make sure the entire line is used
            parseText config
                |. end
    in
    case run parser text of
        Ok parsed ->
            String.split "---" parsed
                |> List.filter (not << String.isEmpty << String.trim)

        Err e ->
            [ "ERROR could not parse: " ++ text ]


{-| Call this as soon as possible and deal with errors appropriately to ensure you
will have no parsing errors later.
-}
parsible : Config a -> String -> Result String ()
parsible config text =
    let
        parser =
            -- make sure the entire line is used
            parseText config
                |. end
    in
    run parser text
        |> Result.map (always ())
        |> Result.mapError Helpers.deadEndsToString


notReserved char =
    not <| List.member char [ '{', '}', '|' ]


staticText : Parser String
staticText =
    succeed ()
        |. chompWhile notReserved
        |> getChompedString
        |> andThen notEmpty


{-| Parses text that looks like "{a|b|c}".

Chooses the option separated by "|" corresponding to the `cycleIndex` in the config (zero-indexed). It sticks on the final option.

Note that empty options are valid, like "{|a||}" which has 3 empty segments.

-- TODO option for loop and maybe random

-}
cyclingText : Config a -> Parser String
cyclingText config =
    let
        findCurrent : List String -> String
        findCurrent l =
            Array.fromList l
                |> Array.get (min (List.length l - 1) config.cycleIndex)
                |> Maybe.withDefault "ERROR finding correct cycling text"

        helper acc =
            oneOf
                [ -- up to here is either "{" or text followed by "|" or "}"
                  -- so if a break or close is found, this is an empty cycle part
                  break |> map (always (Loop <| "" :: acc))
                , close |> map (always (Done <| List.reverse ("" :: acc)))

                --  if it wasn't empty, then it must be some text followed by a break
                --  or close
                , succeed (\a f -> f a)
                    |= lazy (\_ -> parseText config)
                    |= oneOf
                        [ break |> map (always (\t -> Loop (t :: acc)))
                        , close |> map (always (\t -> Done <| List.reverse (t :: acc)))
                        ]
                ]
    in
    loop [] helper
        |> map findCurrent


{-| Parses text that looks like "{myEntity.name}".
Takes a dict keyed by the acceptable keywords (Like "name") with values that take the id and return an appropriate string or error.

You can also use `$.name` which will replace "$" with the value in `config.trigger`.

Note that this means that entity ids cannot use any of the following characters: `.{}|`

-}
propertyText : Config a -> Parser String
propertyText config =
    let
        getProp : String -> (String -> Result String String) -> Result String String
        getProp id propFn =
            propFn <| String.replace "$" config.trigger id

        keywords =
            Dict.toList config.propKeywords
                |> List.map
                    (\( propName, fn ) ->
                        succeed fn
                            |. keyword propName
                    )
    in
    succeed getProp
        |= (getChompedString (chompWhile <| \c -> not <| List.member c [ '{', '.', '|', '}' ])
                |> andThen notEmpty
           )
        |. symbol "."
        |= oneOf keywords
        |. close
        |> andThen fromResult


{-| Parses text that looks like "{myEntity.query? Only shown if true|Optional text to show otherwise}", where "query" can be any syntax query.

You can have multiple queries separated by "&" before the "?". You can also do `*.query` style queries. Any whitespace around after the "?" and around the "|" will be considered part of the text to display.

-}
conditionalText : Config a -> Parser String
conditionalText config =
    let
        assert matcher =
            query matcher config.worldModel
                |> List.isEmpty
                |> not

        process ( queryText, ifText, elseText ) =
            String.split "&" queryText
                |> List.map String.trim
                |> parseMultiple parseMatcher
                |> Result.map
                    (List.all
                        (replaceTrigger config.trigger >> assert)
                    )
                |> (\final ->
                        case final of
                            Ok True ->
                                commit ifText

                            Ok False ->
                                commit elseText

                            Err e ->
                                problem e
                   )
    in
    succeed (\a b c -> ( a, b, c ))
        |= (getChompedString (chompUntil "?") |> andThen notEmpty)
        |. symbol "?"
        -- NOTE add this in to allow whitespace after "?"
        -- makes {x.tag ? yes|no} look nicer, but prevents conditional whitespace
        -- |. chompWhile ((==) ' ')
        |= lazy (\_ -> parseText config)
        |= oneOf
            [ succeed identity
                |. break
                |= lazy (\_ -> parseText config)
                |. close
            , close |> map (always "")
            ]
        |> andThen process


fromResult res =
    case res of
        Ok s ->
            succeed s

        Err e ->
            problem e


open =
    symbol "{"


break =
    symbol "|"


close =
    symbol "}"


parseText : Config a -> Parser String
parseText config =
    let
        topLevel =
            oneOf
                [ succeed identity
                    |. open
                    |= oneOf
                        -- this order is important (because props are a more specific
                        -- version of cycles)
                        [ backtrackable <| conditionalText config
                        , backtrackable <| propertyText config
                        , backtrackable <| cyclingText config
                        ]
                , staticText
                ]

        join : String -> String -> Step String String
        join next base =
            Loop <| next ++ base

        l : String -> Parser (Step String String)
        l base =
            oneOf
                [ map (join base) topLevel

                -- no `end` here because parseText will be used recursively in
                -- bracketed text
                , succeed (Done base)
                ]
    in
    succeed identity
        |= loop "" l
