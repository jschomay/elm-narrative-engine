module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type Item a
    = Item a String


type alias Items a =
    List (Item a)


inventory : Items a -> Html msg
inventory items =
    let
        inventoryItem (Item _ name) =
            li [ class "Item" ]
                [ text name ]
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ ol []
                    (List.map inventoryItem items)
                ]
            ]
