module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (join)
import Color exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)


type Msg a b
    = InteractWithProp a
    | InteractWithCharacter b


currentSummary : DisplayInfo a b c -> StoryState a b c d e -> (StoryElement a b c -> Bool) -> Html (Msg a c)
currentSummary displayInfo storyState beenThereDoneThat =
    let
        currentLocation =
            storyState.currentLocation

        isEmpty =
            (List.length <| getCharactersByLocation currentLocation storyState)
                + (List.length <| getItemsByLocation currentLocation storyState)
                |> (==) 0

        locationName =
            .name <| displayInfo.locations currentLocation

        storyElementDom storyElement =
            let
                classes =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat storyElement )
                    ]

                storyElementName =
                    case storyElement of
                        Item item ->
                            .name <| displayInfo.items item

                        Character character ->
                            .name <| displayInfo.characters character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)

                storyElementMsg =
                    case storyElement of
                        Item item ->
                            InteractWithProp item

                        Character character ->
                            InteractWithCharacter character

                        x ->
                            Debug.crash <| "Error: only characters and items should appear here, got " ++ (toString x)
            in
                span
                    [ classList <| classes
                    , onClick <| storyElementMsg
                    ]
                    [ text <| storyElementName ]

        format list =
            let
                storyElements =
                    if List.length list > 2 then
                        (List.take (List.length list - 1) list
                            |> List.intersperse (text ", ")
                        )
                            ++ (text " and ")
                            :: (List.drop (List.length list - 1) list)
                    else
                        List.intersperse (text " and ") list
            in
                storyElements ++ [ text "." ]

        toCssColor : Color -> String
        toCssColor =
            toRgb >> \{ red, green, blue } -> String.join "" [ "rgb(", toString red, ",", toString green, ",", toString blue, ")" ]

        cssColor =
            toCssColor <| .color <| displayInfo.locations currentLocation

        charactersList =
            if not <| List.isEmpty <| getCharactersByLocation currentLocation storyState then
                getCharactersByLocation currentLocation storyState
                    |> List.map (storyElementDom << Character)
                    |> format
                    |> (::) (text "Characters here: ")
                    |> p []
            else
                span [] []

        itemsList =
            if not <| List.isEmpty <| getItemsByLocation currentLocation storyState then
                getItemsByLocation currentLocation storyState
                    |> List.map (storyElementDom << Item)
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
