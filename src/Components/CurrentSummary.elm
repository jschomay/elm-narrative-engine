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

        charactersDescription =
            let
                charactersPresent =
                    getCharactersByLocation currentLocation storyState

                propElement character =
                    span
                        [ class "Character"
                        , onClick <| InteractWithStage character
                        ]
                        [ text <| getName storyElements character ]
            in
                if List.length charactersPresent < 1 then
                    span [] []
                else
                    List.map propElement charactersPresent
                        |> format
                        |> p []

        propsDescription =
            let
                itemsPresent =
                    getItemsByLocation currentLocation storyState

                propElement item =
                    span
                        [ class "Item"
                        , onClick <| InteractWithStage item
                        ]
                        [ text <| getName storyElements item ]
            in
                if List.length itemsPresent < 1 then
                    span [] []
                else
                    List.map propElement itemsPresent
                        |> format
                        |> p []

        format list =
            if List.length list > 1 then
                (List.take (List.length list - 1) list
                    |> List.intersperse (text ", ")
                )
                    ++ (text ", and ")
                    :: (List.drop (List.length list - 1) list)
                    |> (flip (++))
                        [ text
                            <| if List.length list > 1 then
                                " are here."
                               else
                                " is here."
                        ]
            else
                List.intersperse (text ", ") list
                    |> (flip (++))
                        [ text
                            <| if List.length list > 1 then
                                " are here."
                               else
                                " is here."
                        ]
    in
        div [ class "CurrentSummary" ]
            <| [ h2 [] [ text locationName ]
               , p [] [ text locationDescription ]
               , charactersDescription
               , propsDescription
               ]
