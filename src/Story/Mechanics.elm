module Story.Mechanics exposing (..)

import Story.Element exposing (..)
import Story.State exposing (..)
import Story.Rule exposing (..)


type Msg a b c
    = Interact (Element a b c)


update : Elements a b c -> (d -> Scene a b c d e) -> Msg a b c -> StoryState a b c d e -> StoryState a b c d e
update displayInfo scenes msg storyState =
    let
        defaultNarration element storyState =
            { storyState | storyLine = ( toName element, toDescription element ) :: storyState.storyLine }

        goToLocation element storyState =
            case element of
                Location location ->
                    { storyState | currentLocation = location }

                _ ->
                    Debug.crash "It should be impossible for a non-location element to get here"

        addFamiliarity element storyState =
            { storyState
                | familiarWith =
                    if not <| List.member element storyState.familiarWith then
                        element :: storyState.familiarWith
                    else
                        storyState.familiarWith
            }

        toName element =
            case element of
                Item item ->
                    .name <| displayInfo.items item

                Location location ->
                    .name <| displayInfo.locations location

                Character character ->
                    .name <| displayInfo.characters character

        toDescription element =
            case element of
                Item item ->
                    .description <| displayInfo.items item

                Location location ->
                    .description <| displayInfo.locations location

                Character character ->
                    .description <| displayInfo.characters character

        tryUpdatingFromRules element storyState =
            let
                scene =
                    (scenes storyState.currentScene)

                beenThereDoneThat =
                    (List.member element storyState.familiarWith)
            in
                findMatchingRule element scene storyState beenThereDoneThat
                    `Maybe.andThen` (Just << Story.State.advanceStory (toName element) storyState)
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
