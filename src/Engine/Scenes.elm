module Engine.Scenes
    exposing
        ( init
        , getNarration
        , getCurrentScene
        , update
        , findMatchingRule
        )

import Types exposing (..)
import Dict exposing (Dict)
import List.Zipper
import Engine.Manifest exposing (..)


-- Model


init : List ( String, List ( String, Rule ) ) -> Scenes
init scenes =
    let
        toLiveRule { interaction, conditions, changes, narration } =
            LiveRule interaction
                conditions
                changes
                (narration
                    |> List.map Just
                    |> List.Zipper.fromList
                    |> List.Zipper.withDefault Nothing
                )

        insertRuleFn ( id, rule ) acc =
            Dict.insert id (toLiveRule rule) acc

        insertSceneFn ( id, rules ) acc =
            Dict.insert id (List.foldl insertRuleFn Dict.empty rules) acc
    in
        List.foldl insertSceneFn Dict.empty scenes


getNarration : LiveRule -> Maybe String
getNarration rule =
    List.Zipper.current rule.narration


getCurrentScene : String -> Scenes -> Scene
getCurrentScene sceneName scenes =
    Dict.get sceneName scenes
        |> Maybe.withDefault Dict.empty



-- Update


update : String -> String -> Scenes -> Scenes
update sceneId ruleId scenes =
    let
        updateRuleFn rule =
            { rule | narration = List.Zipper.next rule.narration |> Maybe.withDefault rule.narration }

        updateRulesFn rules =
            Dict.update ruleId (Maybe.map updateRuleFn) rules
    in
        Dict.update sceneId (Maybe.map updateRulesFn) scenes


findMatchingRule :
    { interactableId : String
    , currentLocationId : String
    , manifest : Manifest
    , rules : Scene
    }
    -> Maybe ( String, LiveRule )
findMatchingRule { interactableId, currentLocationId, manifest, rules } =
    Dict.filter
        (matchesRule
            { interactableId = interactableId
            , currentLocationId = currentLocationId
            , manifest = manifest
            }
        )
        rules
        |> Dict.toList
        |> List.head


matchesRule :
    { interactableId : String
    , currentLocationId : String
    , manifest : Manifest
    }
    -> String
    -> LiveRule
    -> Bool
matchesRule { interactableId, currentLocationId, manifest } ruleId rule =
    matchesInteraction manifest rule.interaction interactableId
        && List.all (matchesCondition currentLocationId manifest) rule.conditions


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
    String
    -> Manifest
    -> Condition
    -> Bool
matchesCondition currentLocationId manifest condition =
    case condition of
        ItemIsInInventory item ->
            itemIsInInventory item manifest

        CharacterIsPresent character ->
            characterIsPresent character currentLocationId manifest

        ItemIsPresent item ->
            itemIsPresent item currentLocationId manifest

        IsInLocation location ->
            currentLocationId == location

        ItemIsNotInInventory item ->
            not <| itemIsInInventory item manifest

        CharacterIsNotPresent character ->
            not <| characterIsPresent character currentLocationId manifest

        ItemIsNotPresent item ->
            not <| itemIsPresent item currentLocationId manifest

        IsNotInLocation location ->
            not <| currentLocationId == location
