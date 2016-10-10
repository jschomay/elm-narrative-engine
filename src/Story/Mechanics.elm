module Story.Mechanics exposing (..)

import Story.State exposing (..)
import Types exposing (..)
import Dict


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
            { newStoryState | storyLine = ( name, description ) :: newStoryState.storyLine }

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

        name =
            case displayable of
                Item item ->
                    .name <| displayInfo.items item

                Location location ->
                    .name <| displayInfo.locations location

                Character character ->
                    .name <| displayInfo.characters character

        description =
            case displayable of
                Item item ->
                    .description <| displayInfo.items item

                Location location ->
                    .description <| displayInfo.locations location

                Character character ->
                    .description <| displayInfo.characters character

        defaultUpdate =
            case displayable of
                Item _ ->
                    addDefaultNarration

                Character _ ->
                    addDefaultNarration

                Location _ ->
                    goToLocation
                        >> addDefaultNarration

        getNarration : RuleIndex -> List String -> String
        getNarration ruleIndex narrations =
            let
                currentRuleCount =
                    Dict.get ruleIndex storyState.matchedRules
                        |> Maybe.withDefault 0

                lastNarration =
                    List.drop (List.length narrations - 1) narrations
                        |> List.head
                        |> Maybe.withDefault description
            in
                case narrations of
                    [] ->
                        description

                    _ ->
                        List.drop currentRuleCount narrations
                            |> List.head
                            |> Maybe.withDefault lastNarration

        updateMatchedRulesCount ruleIndex newStoryState =
            let
                newMatchedRules =
                    Dict.get ruleIndex newStoryState.matchedRules
                        |> Maybe.withDefault 0
                        |> \count -> Dict.insert ruleIndex (count + 1) newStoryState.matchedRules
            in
                { newStoryState | matchedRules = newMatchedRules }
    in
        (case findMatchingRule 0 displayable storyState.currentScene storyState of
            Nothing ->
                defaultUpdate storyState

            Just ( ruleIndex, rule ) ->
                Story.State.advanceStory name storyState rule.changes (getNarration ruleIndex rule.narration)
                    |> updateMatchedRulesCount ruleIndex
        )
            |> addFamiliarity
