module Mechanics exposing (..)

import StoryElements exposing (..)
import StoryState exposing (..)
import StoryRules exposing (..)


type Msg a b c
    = Interact (StoryElement a b c)


update : DisplayInfo a b c -> (d -> Scene a b c d e) -> Msg a b c -> StoryState a b c d e -> StoryState a b c d e
update displayInfo scenes msg storyState =
    let
        defaultNarration storyElement storyState =
            { storyState | storyLine = ( toName storyElement, toDescription storyElement ) :: storyState.storyLine }

        goToLocation storyElement storyState =
            case storyElement of
                Location location ->
                    { storyState | currentLocation = location }

                _ ->
                    Debug.crash "It should be impossible for a non-location element to get here"

        addFamiliarity storyElement storyState =
            { storyState
                | familiarWith =
                    if not <| List.member storyElement storyState.familiarWith then
                        storyElement :: storyState.familiarWith
                    else
                        storyState.familiarWith
            }

        toName storyElement =
            case storyElement of
                Item item ->
                    .name <| displayInfo.items item

                Location location ->
                    .name <| displayInfo.locations location

                Character character ->
                    .name <| displayInfo.characters character

        toDescription storyElement =
            case storyElement of
                Item item ->
                    .description <| displayInfo.items item

                Location location ->
                    .description <| displayInfo.locations location

                Character character ->
                    .description <| displayInfo.characters character

        tryUpdatingFromRules storyElement storyState =
            let
                scene =
                    (scenes storyState.currentScene)

                beenThereDoneThat =
                    (List.member storyElement storyState.familiarWith)
            in
                findMatchingRule storyElement scene storyState beenThereDoneThat
                    `Maybe.andThen` (Just << StoryState.advanceStory (toName storyElement) storyState)
    in
        case msg of
            Interact ((Item _) as item) ->
                storyState
                    |> tryUpdatingFromRules item
                    |> Maybe.withDefault (defaultNarration item storyState)
                    |> addFamiliarity item

            Interact ((Location _) as location) ->
                storyState
                    |> tryUpdatingFromRules location
                    |> Maybe.withDefault (goToLocation location storyState |> defaultNarration location)
                    |> addFamiliarity location

            Interact ((Character _) as character) ->
                storyState
                    |> tryUpdatingFromRules character
                    |> Maybe.withDefault (defaultNarration character storyState)
                    |> addFamiliarity character
