module Components.Locations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type Location a
    = Location a String Bool


type alias Locations a =
    List (Location a)


locations : Locations a -> Html msg
locations locations =
    let
        locationItem (Location _ name available) =
            li
                [ classList
                    [ ( "Location", True )
                    , ( "Location--unavailable", not available )
                    ]
                ]
                [ text name ]
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Locations" ]
            , div [ class "Locations__list" ]
                [ ol []
                    (List.map locationItem locations)
                ]
            ]
