module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


inventory : String -> Html a
inventory model =
    div [ class "Inventory" ]
        [ h3 [] [ text "Inventory" ]
        , div [ class "Inventory__list" ]
            [ ol []
                [ li [] [ text "Shiny marble" ]
                , li [] [ text "Torn photograph" ]
                , li [] [ text "Umbrella" ]
                ]
            ]
        ]
