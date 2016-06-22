module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


currentSummary : m -> Html msg
currentSummary model =
    div [ class "CurrentSummary" ]
        [ h2 [] [ text "Rocky marsh" ]
        , p []
            [ text "A difficult place to walk, and not the best smell either.  "
            , span [ class "Character" ] [ text "Mr. Barrowmore" ]
            , text " is here.  "
            , span [ class "Item" ] [ text "Some pebbles" ]
            , text ", "
            , span [ class "Item" ] [ text "moss" ]
            , text " and "
            , span [ class "Item" ] [ text "a broken pair of spectacles" ]
            , text " are here."
            ]
        ]
