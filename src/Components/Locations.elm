module Components.Locations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type KnownLocation a
    = KnownLocation a String Bool


type alias KnownLocations a =
    List (KnownLocation a)


locations : KnownLocations a -> Html msg
locations locations =
    let
        locationItem (KnownLocation _ name available) =
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
