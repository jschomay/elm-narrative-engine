module Components.Locations exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithLocation a


locations : LocationsInfo b -> List b -> b -> (StoryElement a b c -> Bool) -> Html (Msg b)
locations locationsInfo locations currentLocation beenThereDoneThat =
    let
        classes location =
            classList
                [ ( "Locations__Location", True )
                , ( "Locations__Location--current", location == currentLocation )
                , ( "u-selectable", True )
                , ( "u-jump", not <| beenThereDoneThat (Location location) )
                , ( "u-new-story-element", not <| beenThereDoneThat (Location location) )
                ]

        numLocations =
            List.length locations

        locationItem i location =
            let
                key =
                    (toString location) ++ (toString <| numLocations - i)

                cssColor =
                    toCssColor <| getColor <| locationsInfo currentLocation
            in
                ( key
                , li
                    ([ classes location
                     , onClick <| InteractWithLocation location
                     ]
                        ++ if location == currentLocation then
                            [ style [ ( "backgroundColor", cssColor ) ] ]
                           else
                            []
                    )
                    [ text <| getName <| locationsInfo location ]
                )
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Known locations" ]
            , div [ class "Locations__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap locationItem locations)
                ]
            ]
