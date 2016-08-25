module Components.Locations exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithLocation a


locations : StoryElementsConfig a -> List a -> a -> Html (Msg a)
locations storyElements locations currentLocation =
    let
        classes location =
            classList
                [ ( "Locations__Location", True )
                , ( "Locations__Location--current", location == currentLocation )
                , ( "u-selectable", location /= currentLocation )
                , ( "u-jump", location /= currentLocation )
                ]

        numLocations =
            List.length locations

        locationItem i location =
            let
                key =
                    (toString location) ++ (toString <| numLocations - i)
            in
                ( key
                , li
                    ([ classes location ]
                        ++ if location /= currentLocation then
                            [ onClick <| InteractWithLocation location ]
                           else
                            []
                    )
                    [ text <| getName storyElements location ]
                )
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Locations" ]
            , div [ class "Locations__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap locationItem locations)
                ]
            ]
