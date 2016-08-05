module Components.Locations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryWorld exposing (StoryWorld, getName)


type Msg a
    = InteractWithLocation a

locations : StoryWorld -> List a -> Html (Msg a)
locations storyWorld locations =
    let
        locationItem tag =
            li
                [ class "Location"
                , onClick <| InteractWithLocation tag
                ]
                [ text <| getName storyWorld tag]
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Locations" ]
            , div [ class "Locations__list" ]
                [ ol []
                    (List.map locationItem locations)
                ]
            ]
