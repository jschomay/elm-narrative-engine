module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Item storyItem
    = Item storyItem String


type alias Items storyItem =
    List (Item storyItem)


type Msg storyItem
    = InteractWithItem storyItem


inventory : Items a -> Html (Msg a)
inventory items =
    let
        inventoryItem (Item storyItem name) =
            li
                [ class "Item"
                , onClick <| InteractWithItem storyItem
                ]
                [ text name ]
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ ol []
                    (List.map inventoryItem items)
                ]
            ]
