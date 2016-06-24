module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Item a
    = Item a String


type alias Items a =
    List (Item a)


type Msg itemType
    = InteractWithItem itemType


inventory : Items a -> Html (Msg a)
inventory items =
    let
        inventoryItem (Item itemType name) =
            li
                [ class "Item"
                , onClick <| InteractWithItem itemType
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
