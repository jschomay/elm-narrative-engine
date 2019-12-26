module NarrativeEngine.Utils.Helpers exposing (ParseErrors, deadEndsToString, notEmpty, parseErrorsView, parseMultiple)

import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing (..)


type alias ParseErrors =
    List ( String, String )


{-| Generic helper to parse a list against a supplied parse function. Will be `Err`
if any items fail to parse, or an `Ok` of the list of parsed results. Useful for parsing a rules conditions and changes for example.
-}
parseMultiple : (String -> Result String a) -> List String -> Result String (List a)
parseMultiple parser strings =
    List.map parser strings |> sequence


notEmpty : String -> Parser String
notEmpty s =
    if String.isEmpty s then
        problem "cannot be empty"

    else
        succeed s


{-| A helper to show all parse errors. You should identify all possible parse errors as soon as possible, ideally at the top elm `Program` level, and show this view if you have errors.

The tuple is a string identifying the source of the error, and the atual error string iteself.

-}
parseErrorsView : ParseErrors -> Html msg
parseErrorsView errors =
    div
        [ style "background" "black"
        , style "color" "red"
        , style "padding" "4em"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ h1 [] [ text "Errors when parsing!  Please fix:" ]
        , ul [ style "width" "100%" ] <|
            List.map
                (\( source, error ) ->
                    li
                        [ style "margin-bottom" "2em"
                        ]
                        [ pre
                            [ style "background" "white"
                            , style "padding" "1em"
                            , style "color" "black"
                            , style "overflow" " auto"
                            , style "width" "100%"
                            ]
                            [ code [] [ text source ] ]
                        , text error
                        ]
                )
                errors
        ]


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
