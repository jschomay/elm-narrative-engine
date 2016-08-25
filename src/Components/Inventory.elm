module Components.Inventory exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)


type Msg a
    = InteractWithItem a


inventory : StoryElementsConfig a -> List a -> (a -> Bool) -> Html (Msg a)
inventory storyElements items beenThereDoneThat =
    let
        numItems =
            List.length items

        inventoryItem i item =
            let
                key =
                    (toString item) ++ (toString <| numItems - i)

                classes =
                    [ ( "Inventory__Item u-selectable u-jump", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat item )
                    ]
            in
                ( key
                , li
                    [ classList classes
                    , onClick <| InteractWithItem item
                    ]
                    [ text <| getName storyElements item ]
                )
    in
        div [ class "Inventory" ]
            [ h3 [] [ text "Inventory" ]
            , div [ class "Inventory__list" ]
                [ Html.Keyed.ol []
                    (List.indexedMap inventoryItem items)
                ]
            ]
