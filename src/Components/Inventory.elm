module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (StoryElements, getName)


type Msg a
    = InteractWithItem a


inventory : StoryElements -> List a -> Html (Msg a)
inventory storyElements items =
    let
        inventoryItem tag =
            li
                [ class "Item"
                , onClick <| InteractWithItem tag
                ]
                [ text <| getName storyElements tag ]
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ ol []
                    (List.map inventoryItem items)
                ]
            ]
