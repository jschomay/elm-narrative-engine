module Views.Locations exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (..)
import Types exposing (..)


locations :
    (location -> msg)
    -> (location -> LocationInfo)
    -> List location
    -> location
    -> (Color -> String)
    -> (Interactable item location character -> Bool)
    -> Html msg
locations msg locationsInfo locations currentLocation toCssColor beenThereDoneThat =
    let
        classes location =
            classList
                [ ( "Locations__Location", True )
                , ( "Locations__Location--current", location == currentLocation )
                , ( "u-selectable", True )
                , ( "u-jump", not <| beenThereDoneThat (Location location) )
                , ( "u-new-story-interactable", not <| beenThereDoneThat (Location location) )
                ]

        numLocations =
            List.length locations

        locationItem i location =
            let
                key =
                    (toString location) ++ (toString <| numLocations - i)

                cssColor =
                    toCssColor <| .color <| locationsInfo currentLocation
            in
                ( key
                , li
                    ([ classes location
                     , onClick <| msg location
                     ]
                        ++ if location == currentLocation then
                            [ style [ ( "backgroundColor", cssColor ) ] ]
                           else
                            []
                    )
                    [ text <| .name <| locationsInfo location ]
                )
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Known locations" ]
            , div [ class "Locations__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap locationItem locations)
                ]
            ]
