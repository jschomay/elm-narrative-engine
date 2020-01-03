module NarrativeEngine.Utils.NarrativeParser exposing (Narrative, Config, parse, parseMany)

{-| This module is technically outside of the scope of the Narrative Engine, as it deals with content that is handled within your application code, but narrative content is so common that a useful syntax and parser is included here.

Example syntax:

```text
You shout at {$.name}{| again}.
"{What do you want?|Why do you keep bothering me?|Leave me alone!}"
{$.suspicious>3 & WARRANT.location=PLAYER?"Stop!---You're under arrest!"|"Never mind."}
```

The syntax is inspired by Ink (<https://github.com/inkle/ink/blob/master/Documentation/WritingWithInk.md#6-variable-text>).


## Syntax


### Continuation marks

`The door creaks open, and you see... --- Nothing at all!`

Breaks up your narrative into multiple parts (at `---`) for you to render as desired.


### Cycling text

`You have tried this {one|two|three|too many} times.`

Chooses the option separated by `|` corresponding to the `cycleIndex` (zero-indexed) in the `Config`. It repeats the final option after exhausting segments by default, or loops repeatedly with `{~a|b|c}` syntax or chooses a random index each time with `{?a|b|c}` syntax.

The random index is based on the `cycleIndex` and the length of the trigger word and is only quasi random.

Empty segments are allowed - `{|a||b|}` has three empty segments, beginning, middle and end.


### Conditional text

`You feel{PLAYER.happy_level>5? happy| sad}.`

You can use any query syntax from `NarrativeEngine.Utils.RuleParser.parseMatcher`. If it succeeds, it will show the text to the left of `|`, if it fails, it will show the text to the right of the `|`.

`$` will be replaced with the `trigger` in the `Config`.

You can have multiple queries separated by `&` before the `?`. Any white space after the `?` and around the `|` will be considered part of the text to display.

You can leave out the `|` and right side text if you don't want to show any alternative text if the query doesn't pass.


### Custom functions

`The stranger's name is {STRANGER.get_name}.`

Uses `propKeywords` in the `Config` to attempt to run a function (in this case the function keyed with `get_name`) with the entity ID. You can define any kind of function you want, from a simple property lookup (especially useful when using the Entity Component System pattern) to generating a list of items in the supplied location.

You can also use `$.get_name` which will replace `$` with the `trigger` in `Config`.


## Parsing

@docs Narrative, Config, parse, parseMany

-}

import Array
import Dict exposing (Dict)
import NarrativeEngine.Core.WorldModel exposing (..)
import NarrativeEngine.Utils.Helpers as Helpers exposing (ParseErrors, notEmpty, parseMultiple)
import NarrativeEngine.Utils.RuleParser exposing (parseMatcher)
import Parser exposing (..)
import Random
import Result


{-| A fully parsed string, split at the "continue" marks (`---`).
-}
type alias Narrative =
    List String


{-| Provides a context for the parser to process narrative correctly. By managing this in the client, this module can stay stateless and generic.

Includes the following keys:

`cycleIndex` - an integer starting at 0 indicating which index of cycle text should be used (used for all cycles in the narrative string).

`propKeywords` - a dictionary of valid keywords to match against, and the corresponding functions that will take an entity ID and return a string as a Result. If it returns an `Err`, the match will fail.

`worldModel` - the full world model. Used to query against for conditional text.

`trigger` - an entity ID used to replace "$" in conditional text.

-}
type alias Config a =
    { cycleIndex : Int
    , propKeywords : Dict String (String -> Result String String)
    , worldModel : WorldModel a
    , trigger : ID
    }


{-| Parses a single string of narrative content, then splits for continues. Trims and filters out empty strings.

Unlike when parsing all rules or entities ahead of time, you should call this per each individual content each time you have a new narrative. This is because the parsing depends on the `Config`'s state at the time it is parsed and cannot be done ahead of time.

`parseMany` is provided so that you can still check for any parsing errors upon your app initialization.

Note that this will always return a string. In the case of a parsing error, it will say "Error". If you have run `parseMany` initially, then you will not run into this case.

-}
parse : Config a -> String -> Narrative
parse config text =
    case run (top config) text of
        Ok parsed ->
            String.split "---" parsed
                |> List.filter (not << String.isEmpty << String.trim)

        Err e ->
            [ "ERROR could not parse: " ++ text ]


{-| Call this as soon as possible and deal with errors appropriately to ensure you will have no parsing errors later. You can display the errors with `NarrativeEngine.Utils.Helpers.parseErrorsView`.

The provided dictionary should have keys to identify the correlating narrative content values.

If everything parses correctly, you can disregard the resulting empty value, and run `parse` on individual content when needed.

-}
parseMany : Dict String String -> Result ParseErrors ()
parseMany content =
    let
        emptyConfig =
            { cycleIndex = 0
            , propKeywords = Dict.empty
            , trigger = ""
            , worldModel = Dict.empty
            }

        displayError k v e =
            ( "Narrative content: " ++ k ++ "\n" ++ v ++ " ", Helpers.deadEndsToString e )
    in
    Dict.foldl
        (\k v acc ->
            case run (top emptyConfig) v of
                Ok _ ->
                    acc

                Err e ->
                    case acc of
                        Ok _ ->
                            Err [ displayError k v e ]

                        Err errors ->
                            Err <| displayError k v e :: errors
        )
        (Ok ())
        content


{-| Entry point for parsing. Makes sure the entire line is used.
-}
top : Config a -> Parser String
top config =
    parseText config
        |. end


notReserved char =
    not <| List.member char [ '{', '}', '|' ]


staticText : Parser String
staticText =
    succeed ()
        |. chompWhile notReserved
        |> getChompedString
        |> andThen notEmpty


type CycleType
    = Sticking
    | Looping
    | Randomly


{-| Parses text that looks like "{a|b|c}".

Chooses the option separated by "|" corresponding to the `cycleIndex` in the config (zero-indexed). It sticks on the final option by default, or repeats with `{~a|b|c}` syntax or chooses a random index with `{?|a|b|c}` syntax.

Note that empty options are valid, like "{|a||}" which has 3 empty segments.

-}
cyclingText : Config a -> Parser String
cyclingText config =
    let
        findCurrent : CycleType -> List String -> String
        findCurrent cycleType l =
            case cycleType of
                Randomly ->
                    (config.cycleIndex * String.length config.trigger)
                        |> Random.initialSeed
                        |> Random.step (Random.int 0 200)
                        |> Tuple.first
                        |> modBy (List.length l)
                        |> (\i -> Array.get i (Array.fromList l))
                        |> Maybe.withDefault "ERROR finding correct cycling text"

                Looping ->
                    Array.fromList l
                        |> Array.get (modBy (List.length l) config.cycleIndex)
                        |> Maybe.withDefault "ERROR finding correct cycling text"

                Sticking ->
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
    succeed findCurrent
        |= oneOf
            [ symbol "~" |> map (always Looping)
            , symbol "?" |> map (always Randomly)
            , succeed Sticking
            ]
        |= loop [] helper


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
