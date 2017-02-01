module Engine.Rules
    exposing
        ( findMatchingRule
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

        BeenThereDoneThat id ->
            List.member id history

        NotBeenThereDoneThat id ->
            not <| List.member id history

        CurrentSceneIs id ->
            currentSceneId == id
