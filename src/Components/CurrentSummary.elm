module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)


type Msg a
    = InteractWithStage a


currentSummary : StoryElementsConfig a -> StoryState a b -> Html (Msg a)
currentSummary storyElements storyState =
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

                propElement character =
                    span
                        [ class "CurrentSummary__StoryElement"
                        , onClick <| InteractWithStage character
                        ]
                        [ text <| getName storyElements character ]
            in
                if List.length propsAndCharactersPresent < 1 then
                    span [] []
                else
                    List.map propElement propsAndCharactersPresent
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
                (text <| "In location: ")
                    :: storyElements
                    ++ [ text "." ]
    in
        div [ class "CurrentSummary" ]
            <| [ p [ class "Location-description" ] [ text locationDescription ]
               , propsAndCharactersInLocation
               ]
