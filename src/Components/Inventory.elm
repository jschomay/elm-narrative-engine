module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithItem a


inventory : ItemsInfo a -> List a -> (StoryElement a b c -> Bool) -> Html (Msg a)
inventory itemsInfo items beenThereDoneThat =
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
                    , ( "u-new-story-element", not <| beenThereDoneThat (Item item) )
                    ]
            in
                ( key
                , li
                    [ classList classes
                    , onClick <| InteractWithItem item
                    ]
                    [ text <| getName <| itemsInfo item ]
                )
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap inventoryItem items)
                ]
            ]
