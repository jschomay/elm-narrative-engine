module Engine.Rules
    exposing
        ( findMatchingRule
        , bestMatch
        )

import Types exposing (..)
import Dict exposing (Dict)
import Engine.Manifest exposing (..)


-- Model


findMatchingRule :
    { interactableId : String
    , currentLocationId : String
    , currentSceneId : String
    , manifest : Manifest
    , rules : Rules
    , history : List ID
    }
    -> Maybe ( String, Rule )
findMatchingRule { interactableId, currentLocationId, currentSceneId, manifest, rules, history } =
    Dict.filter
        (matchesRule
            { interactableId = interactableId
            , currentLocationId = currentLocationId
            , currentSceneId = currentSceneId
            , manifest = manifest
            , history = history
            }
        )
        rules
        |> Dict.toList
        |> bestMatch


{-| Feed two functions the same value and add their results. Like a Reader, but adds the results of the functions instead of composing them.
-}
(+>) : (a -> Int) -> (a -> Int) -> (a -> Int)
(+>) f1 f2 a =
    f1 a + f2 a


bestMatch : List ( String, Rule ) -> Maybe ( String, Rule )
bestMatch matchingRules =
    let
        numConstrictions rule =
            rule
                |> .conditions
                |> List.length

        sceneConstraint rule =
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

        specificity rule =
            case rule.interaction of
                WithItem _ ->
                    200

                WithLocation _ ->
                    200

                WithCharacter _ ->
                    200

                WithAnyItem ->
                    100

                WithAnyLocation ->
                    100

                WithAnyCharacter ->
                    100

                WithAnything ->
                    0

        weighting =
            always 0
                +> numConstrictions
                +> sceneConstraint
                +> specificity
    in
        List.sortBy (Tuple.second >> weighting) matchingRules
            |> List.reverse
            |> List.head


matchesRule :
    { interactableId : String
    , currentLocationId : String
    , currentSceneId : String
    , manifest : Manifest
    , history : List ID
    }
    -> String
    -> Rule
    -> Bool
matchesRule { interactableId, currentLocationId, currentSceneId, manifest, history } ruleId rule =
    matchesInteraction manifest rule.interaction interactableId
        && List.all (matchesCondition history currentLocationId currentSceneId manifest) rule.conditions


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

        WithItem item ->
            item == interactableId

        WithLocation location ->
            location == interactableId

        WithCharacter character ->
            character == interactableId


matchesCondition :
    List ID
    -> String
    -> String
    -> Manifest
    -> Condition
    -> Bool
matchesCondition history currentLocationId currentSceneId manifest condition =
    case condition of
        ItemIsInInventory item ->
            itemIsInInventory item manifest

        CharacterIsInLocation character location ->
            characterIsInLocation character location manifest

        ItemIsInLocation item location ->
            itemIsInLocation item location manifest

        CurrentLocationIs location ->
            currentLocationId == location

        ItemIsNotInInventory item ->
            not <| itemIsInInventory item manifest

        CharacterIsNotInLocation character location ->
            not <| characterIsInLocation character location manifest

        ItemIsNotInLocation item location ->
            not <| itemIsInLocation item location manifest

        CurrentLocationIsNot location ->
            not <| currentLocationId == location

        HasPreviouslyInteractedWith id ->
            List.member id history

        HasNotPreviouslyInteractedWith id ->
            not <| List.member id history

        CurrentSceneIs id ->
            currentSceneId == id
