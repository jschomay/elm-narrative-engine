module Components.Locations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


locations : String -> Html a
locations model =
    div [ class "Locations" ]
        [ h3 [] [ text "Locations" ]
        , div [ class "Locations__list" ]
            [ ol []
                [ li [] [ text "Rocky marsh" ]
                , li [] [ text "Greenhouse" ]
                ]
            ]
        ]
