module Story.Mechanics exposing (..)

import Story.State exposing (..)
import Types exposing (..)
import List.Zipper


type Msg item location character
    = Interact (Displayable item location character)


update :
    StoryWorld item location character
    -> Msg item location character
    -> StoryState item location character knowledge
    -> StoryState item location character knowledge
update displayInfo (Interact displayable) storyState =
    let
        addDefaultNarration newStoryState =
            { newStoryState | storyLine = ( getName displayInfo displayable, getDescription displayInfo displayable ) :: newStoryState.storyLine }

        goToLocation newStoryState =
            case displayable of
                Location location ->
                    { newStoryState | currentLocation = location }

                _ ->
                    Debug.crash "It should be impossible for a non-location displayable to get here"

        addFamiliarity newStoryState =
            { newStoryState
                | familiarWith =
                    if not <| List.member displayable newStoryState.familiarWith then
                        displayable :: newStoryState.familiarWith
                    else
                        newStoryState.familiarWith
            }

        updateCurrentScene ruleIndex matchedRule newStoryState =
            let
                nextOrLasttNarration currentNarrationZipper =
                    Maybe.withDefault (List.Zipper.last currentNarrationZipper)
                        (List.Zipper.next currentNarrationZipper)

                updateMatchedRule index liveRule =
                    if index == ruleIndex then
                        { liveRule | narration = Maybe.map nextOrLasttNarration matchedRule.narration }
                    else
                        liveRule

                newCurrentScene =
                    List.indexedMap updateMatchedRule newStoryState.currentScene
            in
                { newStoryState | currentScene = newCurrentScene }

        defaultUpdate =
            case displayable of
                Item _ ->
                    addDefaultNarration

                Character _ ->
                    addDefaultNarration

                Location _ ->
                    goToLocation
                        >> addDefaultNarration
    in
        (case findMatchingRule 0 displayable storyState.currentScene storyState of
            Nothing ->
                defaultUpdate storyState

            Just ( ruleIndex, rule ) ->
                Story.State.advanceStory (getName displayInfo displayable) storyState rule.changes (getNarration (getDescription displayInfo displayable) rule)
                    |> updateCurrentScene ruleIndex rule
        )
            |> addFamiliarity
