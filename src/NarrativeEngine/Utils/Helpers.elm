module NarrativeEngine.Utils.Helpers exposing (ParseError, deadEndsToString, notEmpty, parseMultiple)

import Parser exposing (..)


type alias ParseError =
    List DeadEnd


{-| Generic helper to parse a list against a supplied parse function. Will be `Err`
if any items fail to parse, or an `Ok` of the list of parsed results. Useful for parsing a rules conditions and changes for example.
-}
parseMultiple : (String -> Result ParseError a) -> List String -> Result ParseError (List a)
parseMultiple parser strings =
    List.map parser strings |> sequence


notEmpty : String -> Parser String
notEmpty s =
    if String.isEmpty s then
        problem "cannot be empty"

    else
        succeed s


{-| "Switches" a higher-order "kind"
-}
sequence : List (Result e a) -> Result e (List a)
sequence list =
    List.foldl
        (\r acc ->
            case r of
                Ok a ->
                    Result.map ((::) a) acc

                Err e ->
                    Err e
        )
        (Ok [])
        list



{- Borrowed from https://github.com/elm/parser/pull/16 -}


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString deadend =
            problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col

        problemToString p =
            case p of
                Expecting s ->
                    "expecting '" ++ s ++ "'"

                ExpectingInt ->
                    "expecting int"

                ExpectingHex ->
                    "expecting hex"

                ExpectingOctal ->
                    "expecting octal"

                ExpectingBinary ->
                    "expecting binary"

                ExpectingFloat ->
                    "expecting float"

                ExpectingNumber ->
                    "expecting number"

                ExpectingVariable ->
                    "expecting variable"

                ExpectingSymbol s ->
                    "expecting symbol '" ++ s ++ "'"

                ExpectingKeyword s ->
                    "expecting keyword '" ++ s ++ "'"

                ExpectingEnd ->
                    "expecting end"

                UnexpectedChar ->
                    "unexpected char"

                Problem s ->
                    "problem " ++ s

                BadRepeat ->
                    "bad repeat"
    in
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))
