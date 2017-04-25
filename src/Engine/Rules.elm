module Engine.Rules
    exposing
        ( findMatchingRule
        , bestMatch
        , chooseFrom
        )

import Types exposing (..)
import Dict exposing (Dict)
import Engine.Manifest exposing (..)


findMatchingRule : Story -> String -> Maybe ( String, Rule )
findMatchingRule story interaction =
    story.rules
        |> Dict.toList
        |> List.filter (Tuple.second >> matchesRule story interaction)
        |> List.map
            (\( id, { interaction, conditions, changes } ) ->
                { id = id, interaction = interaction, conditions = conditions, changes = changes }
            )
        |> bestMatch
            (numConstrictionsWeight
                +> sceneConstraintWeight
                +> specificityWeight
            )
        |> Maybe.map
            (\{ id, interaction, conditions, changes } ->
                ( id, { interaction = interaction, conditions = conditions, changes = changes } )
            )


{-| Feed two functions the same value and add their results. Like a Reader, but adds the results of the functions instead of composing them.
-}
(+>) : (a -> Int) -> (a -> Int) -> (a -> Int)
(+>) f1 f2 a =
    f1 a + f2 a


bestMatch : (a -> Int) -> List a -> Maybe a
bestMatch heuristics matchingRules =
    List.sortBy heuristics matchingRules
        |> List.reverse
        |> List.head


numConstrictionsWeight : { a | conditions : List Condition } -> Int
numConstrictionsWeight =
    .conditions >> List.length


sceneConstraintWeight : { a | conditions : List Condition } -> Int
sceneConstraintWeight rule =
    let
        hasSceneConstraints condition =
            case condition of
                CurrentSceneIs _ ->
                    True

                _ ->
                    False
    in
        if List.any hasSceneConstraints rule.conditions then
            300
        else
            0


specificityWeight : { a | interaction : InteractionMatcher } -> Int
specificityWeight rule =
    case rule.interaction of
        With _ ->
            200

        WithAnyItem ->
            100

        WithAnyLocation ->
            100

        WithAnyCharacter ->
            100

        WithAnything ->
            0


chooseFrom : Story -> List { a | conditions : List Condition } -> Maybe { a | conditions : List Condition }
chooseFrom ({ currentLocation, currentScene, manifest, history } as story) =
    List.filter (.conditions >> List.all (matchesCondition story))
        >> bestMatch (numConstrictionsWeight +> sceneConstraintWeight)


matchesRule : Story -> String -> Rule -> Bool
matchesRule ({ currentLocation, currentScene, manifest, history } as story) interaction rule =
    matchesInteraction manifest rule.interaction interaction
        && List.all (matchesCondition story) rule.conditions


matchesInteraction :
    Manifest
    -> InteractionMatcher
    -> String
    -> Bool
matchesInteraction manifest interactionMatcher interactableId =
    case interactionMatcher of
        WithAnything ->
            True

        WithAnyItem ->
            isItem interactableId manifest

        WithAnyLocation ->
            isLocation interactableId manifest

        WithAnyCharacter ->
            isCharacter interactableId manifest

        With id ->
            id == interactableId


matchesCondition :
    Story
    -> Condition
    -> Bool
matchesCondition { history, currentLocation, currentScene, manifest } condition =
    case condition of
        ItemIsInInventory item ->
            itemIsInInventory item manifest

        CharacterIsInLocation character location ->
            characterIsInLocation character location manifest

        ItemIsInLocation item location ->
            itemIsInLocation item location manifest

        CurrentLocationIs location ->
            currentLocation == location

        ItemIsNotInInventory item ->
            not <| itemIsInInventory item manifest

        CharacterIsNotInLocation character location ->
            not <| characterIsInLocation character location manifest

        ItemIsNotInLocation item location ->
            not <| itemIsInLocation item location manifest

        CurrentLocationIsNot location ->
            not <| currentLocation == location

        HasPreviouslyInteractedWith id ->
            List.member id history

        HasNotPreviouslyInteractedWith id ->
            not <| List.member id history

        CurrentSceneIs id ->
            currentScene == id
