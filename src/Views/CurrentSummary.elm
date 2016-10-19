module Views.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (..)
import Story.State exposing (..)
import Types exposing (..)


currentSummary :
    (item -> msg)
    -> (character -> msg)
    -> StoryWorld item location character
    -> (Color -> String)
    -> (Interactable item location character -> Bool)
    -> StoryState item location character knowledge
    -> Html msg
currentSummary itemMsg charcterMsg interactables toCssColor beenThereDoneThat storyState =
    let
        currentLocation =
            storyState.currentLocation

        isEmpty =
            (List.length <| getCharactersByLocation currentLocation storyState)
                + (List.length <| getItemsByLocation currentLocation storyState)
                |> (==) 0

        locationName =
            .name <| interactables.locations currentLocation

        interactableDom interactable =
            let
                classes =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-interactable", not <| beenThereDoneThat interactable )
                    ]

                interactableName =
                    case interactable of
                        Item item ->
                            .name <| interactables.items item

                        Character character ->
                            .name <| interactables.characters character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)

                interactableMsg =
                    case interactable of
                        Item item ->
                            itemMsg item

                        Character character ->
                            charcterMsg character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)
            in
                span
                    [ classList <| classes
                    , onClick <| interactableMsg
                    ]
                    [ text <| interactableName ]

        format list =
            let
                interactables =
                    if List.length list > 2 then
                        (List.take (List.length list - 1) list
                            |> List.intersperse (text ", ")
                        )
                            ++ (text " and ")
                            :: (List.drop (List.length list - 1) list)
                    else
                        List.intersperse (text " and ") list
            in
                interactables ++ [ text "." ]

        cssColor =
            toCssColor <| .color <| interactables.locations currentLocation

        charactersList =
            if not <| List.isEmpty <| getCharactersByLocation currentLocation storyState then
                getCharactersByLocation currentLocation storyState
                    |> List.map (interactableDom << Character)
                    |> format
                    |> (::) (text "Characters here: ")
                    |> p []
            else
                span [] []

        itemsList =
            if not <| List.isEmpty <| getItemsByLocation currentLocation storyState then
                getItemsByLocation currentLocation storyState
                    |> List.map (interactableDom << Item)
                    |> format
                    |> (::) (text "Items here: ")
                    |> p []
            else
                span [] []
    in
        div [ class "CurrentSummary", style [ ( "color", cssColor ) ] ]
            <| if isEmpty then
                []
               else
                [ charactersList, itemsList ]
