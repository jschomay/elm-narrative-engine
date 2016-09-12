module Views.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (..)
import Story.Element exposing (..)
import Story.State exposing (..)


currentSummary : (a -> msg) -> (c -> msg) -> Elements a b c -> (Color -> String) -> (Element a b c -> Bool) -> StoryState a b c d e -> Html msg
currentSummary itemMsg charcterMsg elements toCssColor beenThereDoneThat storyState =
    let
        currentLocation =
            storyState.currentLocation

        isEmpty =
            (List.length <| getCharactersByLocation currentLocation storyState)
                + (List.length <| getItemsByLocation currentLocation storyState)
                |> (==) 0

        locationName =
            .name <| elements.locations currentLocation

        elementDom element =
            let
                classes =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat element )
                    ]

                elementName =
                    case element of
                        Item item ->
                            .name <| elements.items item

                        Character character ->
                            .name <| elements.characters character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)

                elementMsg =
                    case element of
                        Item item ->
                            itemMsg item

                        Character character ->
                            charcterMsg character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)
            in
                span
                    [ classList <| classes
                    , onClick <| elementMsg
                    ]
                    [ text <| elementName ]

        format list =
            let
                elements =
                    if List.length list > 2 then
                        (List.take (List.length list - 1) list
                            |> List.intersperse (text ", ")
                        )
                            ++ (text " and ")
                            :: (List.drop (List.length list - 1) list)
                    else
                        List.intersperse (text " and ") list
            in
                elements ++ [ text "." ]

        cssColor =
            toCssColor <| .color <| elements.locations currentLocation

        charactersList =
            if not <| List.isEmpty <| getCharactersByLocation currentLocation storyState then
                getCharactersByLocation currentLocation storyState
                    |> List.map (elementDom << Character)
                    |> format
                    |> (::) (text "Characters here: ")
                    |> p []
            else
                span [] []

        itemsList =
            if not <| List.isEmpty <| getItemsByLocation currentLocation storyState then
                getItemsByLocation currentLocation storyState
                    |> List.map (elementDom << Item)
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
