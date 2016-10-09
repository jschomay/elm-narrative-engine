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
    -> (Displayable item location character -> Bool)
    -> StoryState item location character knowledge
    -> Html msg
currentSummary itemMsg charcterMsg displayables toCssColor beenThereDoneThat storyState =
    let
        currentLocation =
            storyState.currentLocation

        isEmpty =
            (List.length <| getCharactersByLocation currentLocation storyState)
                + (List.length <| getItemsByLocation currentLocation storyState)
                |> (==) 0

        locationName =
            .name <| displayables.locations currentLocation

        displayableDom displayable =
            let
                classes =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-displayable", not <| beenThereDoneThat displayable )
                    ]

                displayableName =
                    case displayable of
                        Item item ->
                            .name <| displayables.items item

                        Character character ->
                            .name <| displayables.characters character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)

                displayableMsg =
                    case displayable of
                        Item item ->
                            itemMsg item

                        Character character ->
                            charcterMsg character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)
            in
                span
                    [ classList <| classes
                    , onClick <| displayableMsg
                    ]
                    [ text <| displayableName ]

        format list =
            let
                displayables =
                    if List.length list > 2 then
                        (List.take (List.length list - 1) list
                            |> List.intersperse (text ", ")
                        )
                            ++ (text " and ")
                            :: (List.drop (List.length list - 1) list)
                    else
                        List.intersperse (text " and ") list
            in
                displayables ++ [ text "." ]

        cssColor =
            toCssColor <| .color <| displayables.locations currentLocation

        charactersList =
            if not <| List.isEmpty <| getCharactersByLocation currentLocation storyState then
                getCharactersByLocation currentLocation storyState
                    |> List.map (displayableDom << Character)
                    |> format
                    |> (::) (text "Characters here: ")
                    |> p []
            else
                span [] []

        itemsList =
            if not <| List.isEmpty <| getItemsByLocation currentLocation storyState then
                getItemsByLocation currentLocation storyState
                    |> List.map (displayableDom << Item)
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
