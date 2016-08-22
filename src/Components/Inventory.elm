module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithItem a


inventory : StoryElementsConfig a -> List a -> Html (Msg a)
inventory storyElements items =
    let
        inventoryItem storyElement =
            li
                [ class "Inventory__Item"
                , onClick <| InteractWithItem storyElement
                ]
                [ text <| getName storyElements storyElement ]
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ ol []
                    (List.map inventoryItem items)
                ]
            ]
