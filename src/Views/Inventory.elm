module Views.Inventory exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


inventory :
    (item -> msg)
    -> (item -> ItemInfo)
    -> List item
    -> (Interactable item location character -> Bool)
    -> Html msg
inventory msg itemsInfo items beenThereDoneThat =
    let
        numItems =
            List.length items

        inventoryItem i item =
            let
                key =
                    (toString item) ++ (toString <| numItems - i)

                classes =
                    [ ( "Inventory__Item u-selectable", True )
                    , ( "u-jump", not <| beenThereDoneThat (Item item) )
                    , ( "u-new-story-interactable", not <| beenThereDoneThat (Item item) )
                    ]
            in
                ( key
                , li
                    [ classList classes
                    , onClick <| msg item
                    ]
                    [ text <| .name <| itemsInfo item ]
                )
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap inventoryItem items)
                ]
            ]
