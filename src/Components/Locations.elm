module Components.Locations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithLocation a


locations : StoryElementsConfig a -> List a -> Html (Msg a)
locations storyElements locations =
    let
        locationItem tag =
            li
                [ class "Location"
                , onClick <| InteractWithLocation tag
                ]
                [ text <| getName storyElements tag ]
    in
        div [ class "Locations" ]
            [ h3 [] [ text "Locations" ]
            , div [ class "Locations__list" ]
                [ ol []
                    (List.map locationItem locations)
                ]
            ]
