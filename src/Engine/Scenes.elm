module Engine.Scenes
    exposing
        ( init
        , getCurrentScene
        , findMatchingRule
        )

import Types exposing (..)
import Dict exposing (Dict)
import Engine.Manifest exposing (..)


-- Model


init : List ( String, List ( String, Rule ) ) -> Scenes
init scenes =
    let
        insertRuleFn ( id, rule ) acc =
            Dict.insert id rule acc

        insertSceneFn ( id, rules ) acc =
            Dict.insert id (List.foldl insertRuleFn Dict.empty rules) acc
    in
        List.foldl insertSceneFn Dict.empty scenes


getCurrentScene : String -> Scenes -> Scene
getCurrentScene sceneName scenes =
    Dict.get sceneName scenes
        |> Maybe.withDefault Dict.empty


findMatchingRule :
    { interactableId : String
    , currentLocationId : String
    , manifest : Manifest
    , rules : Scene
    , history : List ID
    }
    -> Maybe ( String, Rule )
findMatchingRule { interactableId, currentLocationId, manifest, rules, history } =
    Dict.filter
        (matchesRule
            { interactableId = interactableId
            , currentLocationId = currentLocationId
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
    , manifest : Manifest
    , history : List ID
    }
    -> String
    -> Rule
    -> Bool
matchesRule { interactableId, currentLocationId, manifest, history } ruleId rule =
    matchesInteraction manifest rule.interaction interactableId
        && List.all (matchesCondition history currentLocationId manifest) rule.conditions


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
    -> Manifest
    -> Condition
    -> Bool
matchesCondition history currentLocationId manifest condition =
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
