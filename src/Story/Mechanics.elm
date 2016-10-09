module Story.Mechanics exposing (..)

import Story.Displayable exposing (..)
import Story.State exposing (..)


type Msg item location character
    = Interact (Displayable item location character)


update :
    StoryWorld item location character
    -> Msg item location character
    -> StoryState item location character knowledge
    -> StoryState item location character knowledge
update displayInfo msg storyState =
    let
        defaultNarration displayable storyState =
            { storyState | storyLine = ( toName displayable, toDescription displayable ) :: storyState.storyLine }

        goToLocation displayable storyState =
            case displayable of
                Location location ->
                    { storyState | currentLocation = location }

                _ ->
                    Debug.crash "It should be impossible for a non-location displayable to get here"

        addFamiliarity displayable storyState =
            { storyState
                | familiarWith =
                    if not <| List.member displayable storyState.familiarWith then
                        displayable :: storyState.familiarWith
                    else
                        storyState.familiarWith
            }

        toName displayable =
            case displayable of
                Item item ->
                    .name <| displayInfo.items item

                Location location ->
                    .name <| displayInfo.locations location

                Character character ->
                    .name <| displayInfo.characters character

        toDescription displayable =
            case displayable of
                Item item ->
                    .description <| displayInfo.items item

                Location location ->
                    .description <| displayInfo.locations location

                Character character ->
                    .description <| displayInfo.characters character

        tryUpdatingFromRules displayable storyState =
            findMatchingRule displayable storyState.currentScene storyState
                `Maybe.andThen` \{ changes, narration } ->
                                    Just <| Story.State.advanceStory (toName displayable) storyState changes narration
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
