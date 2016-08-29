module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)
import Markdown


type Msg a b
    = InteractWithProp a
    | InteractWithCharacter b


currentSummary : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> StoryState a b c d -> (StoryElement a b c -> Bool) -> Html (Msg a c)
currentSummary itemsInfo locationsInfo charactersInfo storyState beenThereDoneThat =
    let
        currentLocation =
            storyState.currentLocation

        locationName =
            getName <| locationsInfo currentLocation

        locationDescription =
            getDescription <| locationsInfo currentLocation

        propsAndCharactersInLocation =
            let
                propsAndCharactersPresent =
                    (getCharactersByLocation currentLocation storyState
                        |> List.map (interactableElements << Character)
                    )
                        ++ (getItemsByLocation currentLocation storyState
                                |> List.map (interactableElements << Item)
                           )

                classes storyElement =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat storyElement )
                    ]

                getStoryElementName storyElement =
                    case storyElement of
                        Item item ->
                            getName <| itemsInfo item

                        Location location ->
                            Debug.crash <| "Error: A location should never appear here: " ++ (toString location)

                        Character character ->
                            getName <| charactersInfo character

                getStoryElementMsg storyElement =
                    case storyElement of
                        Item item ->
                            InteractWithProp item

                        Location location ->
                            Debug.crash <| "Error: A location should never appear here: " ++ (toString location)

                        Character character ->
                            InteractWithCharacter character

                interactableElements storyElement =
                    span
                        [ classList <| classes storyElement
                        , onClick <| getStoryElementMsg storyElement
                        ]
                        [ text <| getStoryElementName storyElement ]
            in
                if List.length propsAndCharactersPresent < 1 then
                    span [] []
                else
                    propsAndCharactersPresent
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
