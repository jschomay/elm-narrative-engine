module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryWorld exposing (..)


type Msg a
    = InteractWithStage a


currentSummary : StoryWorld -> a -> List a -> List a -> Html (Msg a)
currentSummary storyWorld location characters props =
    let
        locationName =
            getName storyWorld location

        locationDescription =
            getDescription storyWorld location

        charactersDescription =
            let
                propElement character =
                    span
                        [ class "Character"
                        , onClick <| InteractWithStage character
                        ]
                        [ text <| getName storyWorld character ]
            in
                if List.length characters < 1 then
                    span [] []
                else
                    List.map propElement characters
                        |> format
                        |> p []

        propsDescription =
            let
                propElement item =
                    span
                        [ class "Item"
                        , onClick <| InteractWithStage item
                        ]
                        [ text <| getName storyWorld item ]
            in
                if List.length props < 1 then
                    span [] []
                else
                    List.map propElement props
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
