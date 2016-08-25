module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)
import Markdown


type Msg a
    = InteractWithStage a


currentSummary : StoryElementsConfig a -> StoryState a b -> (a -> Bool) -> Html (Msg a)
currentSummary storyElements storyState beenThereDoneThat =
    let
        currentLocation =
            storyState.currentLocation

        locationName =
            getName storyElements currentLocation

        locationDescription =
            getDescription storyElements currentLocation

        propsAndCharactersInLocation =
            let
                propsAndCharactersPresent =
                    getCharactersByLocation currentLocation storyState
                        ++ getItemsByLocation currentLocation storyState

                classes storyElement =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat storyElement )
                    ]

                interactableElements storyElement =
                    span
                        [ classList <| classes storyElement
                        , onClick <| InteractWithStage storyElement
                        ]
                        [ text <| getName storyElements storyElement ]
            in
                if List.length propsAndCharactersPresent < 1 then
                    span [] []
                else
                    List.map interactableElements propsAndCharactersPresent
                        |> format
                        |> p []

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
                (text <| "Also here: ")
                    :: storyElements
                    ++ [ text "." ]
    in
        div [ class "CurrentSummary" ]
            <| [ p [ class "Location-description" ] [ Markdown.toHtml [] locationDescription ]
               , propsAndCharactersInLocation
               ]
