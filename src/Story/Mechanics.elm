module Story.Mechanics exposing (buildStoryState)

import Story.State exposing (..)
import Types exposing (..)
import List
import List.Zipper


buildStoryState :
    World item location character
    -> { startingState : StoryState item location character knowledge
       , interactions : List (Interaction item location character)
       }
    -> StoryState item location character knowledge
buildStoryState displayInfo { interactions, startingState } =
    List.foldl (step displayInfo) startingState interactions


step :
    World item location character
    -> Interaction item location character
    -> StoryState item location character knowledge
    -> StoryState item location character knowledge
step displayInfo (Interaction interactable) storyState =
    let
        addDefaultNarration newStoryState =
            { newStoryState | storyLine = ( getName displayInfo interactable, getDescription displayInfo interactable ) :: newStoryState.storyLine }

        goToLocation newStoryState =
            case interactable of
                Location location ->
                    { newStoryState | currentLocation = location }

                _ ->
                    Debug.crash "It should be impossible for a non-location interactable to get here"

        addFamiliarity newStoryState =
            { newStoryState
                | familiarWith =
                    if not <| List.member interactable newStoryState.familiarWith then
                        interactable :: newStoryState.familiarWith
                    else
                        newStoryState.familiarWith
            }

        updateCurrentScene ruleIndex newStoryState =
            let
                nextOrLasttNarration currentNarrationZipper =
                    Maybe.withDefault (List.Zipper.last currentNarrationZipper)
                        (List.Zipper.next currentNarrationZipper)

                updateMatchedRule index liveRule =
                    if index == ruleIndex then
                        { liveRule | narration = Maybe.map nextOrLasttNarration liveRule.narration }
                    else
                        liveRule

                newCurrentScene =
                    List.indexedMap updateMatchedRule newStoryState.currentScene
            in
                { newStoryState | currentScene = newCurrentScene }

        defaultUpdate =
            case interactable of
                Item _ ->
                    addDefaultNarration

                Character _ ->
                    addDefaultNarration

                Location _ ->
                    goToLocation
                        >> addDefaultNarration
    in
        (case findMatchingRule 0 interactable storyState.currentScene storyState of
            Nothing ->
                defaultUpdate storyState

            Just ( ruleIndex, rule ) ->
                Story.State.advanceStory (getName displayInfo interactable) storyState rule.changes (getNarration (getDescription displayInfo interactable) rule)
                    |> updateCurrentScene ruleIndex
        )
            |> addFamiliarity
