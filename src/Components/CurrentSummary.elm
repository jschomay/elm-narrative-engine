module Components.CurrentSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StoryElements exposing (..)
import StoryState exposing (..)


type Msg a b
    = InteractWithProp a
    | InteractWithCharacter b


currentSummary : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> StoryState a b c d -> (StoryElement a b c -> Bool) -> Html (Msg a c)
currentSummary itemsInfo locationsInfo charactersInfo storyState beenThereDoneThat =
    let
        currentLocation =
            storyState.currentLocation

        isEmpty =
            (List.length <| getCharactersByLocation currentLocation storyState)
                + (List.length <| getItemsByLocation currentLocation storyState)
                |> (==) 0

        locationName =
            getName <| locationsInfo currentLocation

        storyElementDom storyElement =
            let
                classes =
                    [ ( "CurrentSummary__StoryElement u-selectable", True )
                    , ( "u-new-story-element", not <| beenThereDoneThat storyElement )
                    ]

                storyElementName =
                    case storyElement of
                        Item item ->
                            getName <| itemsInfo item

                        Location location ->
                            Debug.crash <| "Error: A location should never appear here: " ++ (toString location)

                        Character character ->
                            getName <| charactersInfo character

                storyElementMsg =
                    case storyElement of
                        Item item ->
                            InteractWithProp item

                        Location location ->
                            Debug.crash <| "Error: A location should never appear here: " ++ (toString location)

                        Character character ->
                            InteractWithCharacter character
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

        cssColor =
            toCssColor <| getColor <| locationsInfo currentLocation

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
                [ p [] [ text "There is nothing here." ] ]
               else
                [ charactersList, itemsList ]
