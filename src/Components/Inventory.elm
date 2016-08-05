module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryWorld exposing (StoryWorld, getName)


type Msg a
    = InteractWithItem a


inventory : StoryWorld -> List a -> Html (Msg a)
inventory storyWorld items =
    let
        inventoryItem tag =
            li
                [ class "Item"
                , onClick <| InteractWithItem tag
                ]
                [ text <| getName storyWorld tag ]
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ ol []
                    (List.map inventoryItem items)
                ]
            ]
