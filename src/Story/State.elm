module Story.State exposing (..)

import Types exposing (..)
import EveryDict exposing (..)
import List.Zipper


init : location -> List (Rule item location character knowledge) -> StoryState item location character knowledge
init startingLocation startingScene =
    StoryState startingLocation
        (loadCurrentScene startingScene)
        [ Location startingLocation ]
        []
        []
        EveryDict.empty
        EveryDict.empty
        []


getCharactersInCurrentLocation : StoryState item location character knowledge -> List character
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getItemsInCurrentLocation : StoryState item location character knowledge -> List item
getItemsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getInventory : StoryState item location character knowledge -> List item
getInventory storyState =
    let
        find item placement acc =
            case placement of
                Prop _ ->
                    acc

                Inventory ->
                    acc ++ [ item ]
    in
        EveryDict.foldr find [] storyState.itemPlacements


getCharactersByLocation : location -> StoryState item location character knowledge -> List character
getCharactersByLocation location storyState =
    let
        find character location' acc =
            if location' == location then
                acc ++ [ character ]
            else
                acc
    in
        EveryDict.foldr find [] storyState.characterPlacements


getItemsByLocation : location -> StoryState item location character knowledge -> List item
getItemsByLocation location storyState =
    let
        find item placement acc =
            case placement of
                Inventory ->
                    acc

                Prop location' ->
                    if location' == location then
                        acc ++ [ item ]
                    else
                        acc
    in
        EveryDict.foldr find [] storyState.itemPlacements


getName : StoryWorld item location character -> Interactable item location character -> String
getName displayInfo interactable =
    case interactable of
        Item item ->
            .name <| displayInfo.items item

        Location location ->
            .name <| displayInfo.locations location

        Character character ->
            .name <| displayInfo.characters character


getDescription : StoryWorld item location character -> Interactable item location character -> String
getDescription displayInfo interactable =
    case interactable of
        Item item ->
            .description <| displayInfo.items item

        Location location ->
            .description <| displayInfo.locations location

        Character character ->
            .description <| displayInfo.characters character


getNarration : String -> LiveRule item location character knowledge -> String
getNarration default matchedRule =
    matchedRule.narration
        |> Maybe.map List.Zipper.current
        |> Maybe.withDefault default


isItem : Interactable item location character -> Bool
isItem interactable =
    case interactable of
        Item _ ->
            True

        _ ->
            False


isLocation : Interactable item location character -> Bool
isLocation interactable =
    case interactable of
        Location _ ->
            True

        _ ->
            False


isCharacter : Interactable item location character -> Bool
isCharacter interactable =
    case interactable of
        Character _ ->
            True

        _ ->
            False


loadCurrentScene : List (Rule item location character knowledge) -> List (LiveRule item location character knowledge)
loadCurrentScene ruleData =
    let
        toLiveRule rule =
            LiveRule rule.interaction rule.conditions rule.changes (List.Zipper.fromList rule.narration)
    in
        List.map toLiveRule ruleData


advanceStory :
    String
    -> StoryState item location character knowledge
    -> List (ChangeWorldCommand item location character knowledge)
    -> String
    -> StoryState item location character knowledge
advanceStory interactableName storyState changesWorldCommands narration =
    let
        addNarration narration storyState =
            { storyState
                | storyLine = narration :: storyState.storyLine
            }

        doCommand command storyState =
            case command of
                MoveTo location ->
                    moveTo location storyState
                        |> addLocation location

                AddLocation location ->
                    addLocation location storyState

                RemoveLocation location ->
                    removeLocation location storyState

                AddInventory item ->
                    addInventory item storyState

                RemoveInventory item ->
                    removeInventory item storyState

                MoveCharacter character location ->
                    moveCharacter character location storyState

                RemoveCharacter character ->
                    removeCharacter character storyState

                PlaceItem item location ->
                    placeItem item location storyState

                RemoveItem item ->
                    removeItem item storyState

                AddKnowledge knowledge ->
                    addKnowledge knowledge storyState

                LoadScene scene ->
                    loadScene scene storyState

                EndStory ->
                    endStory storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( interactableName, narration )


moveTo : location -> StoryState item location character knowledge -> StoryState item location character knowledge
moveTo location storyState =
    if List.member (Location location) storyState.familiarWith then
        { storyState
            | currentLocation = location
        }
    else
        { storyState
            | currentLocation = location
            , familiarWith = storyState.familiarWith ++ [ Location location ]
        }


addLocation : location -> StoryState item location character knowledge -> StoryState item location character knowledge
addLocation location storyState =
    if List.member location storyState.knownLocations then
        storyState
    else
        { storyState
            | knownLocations = location :: storyState.knownLocations
        }


removeLocation : location -> StoryState item location character knowledge -> StoryState item location character knowledge
removeLocation location storyState =
    { storyState
        | knownLocations = List.filter ((/=) location) storyState.knownLocations
    }


addInventory : item -> StoryState item location character knowledge -> StoryState item location character knowledge
addInventory item storyState =
    { storyState
        | itemPlacements =
            EveryDict.insert item Inventory storyState.itemPlacements
    }


removeItem : item -> StoryState item location character knowledge -> StoryState item location character knowledge
removeItem item storyState =
    { storyState
        | itemPlacements = EveryDict.remove item storyState.itemPlacements
    }


removeInventory : item -> StoryState item location character knowledge -> StoryState item location character knowledge
removeInventory item storyState =
    { storyState
        | itemPlacements = EveryDict.remove item storyState.itemPlacements
    }


moveCharacter : character -> location -> StoryState item location character knowledge -> StoryState item location character knowledge
moveCharacter character location storyState =
    { storyState
        | characterPlacements =
            EveryDict.insert character location storyState.characterPlacements
    }


removeCharacter : character -> StoryState item location character knowledge -> StoryState item location character knowledge
removeCharacter character storyState =
    { storyState
        | characterPlacements = EveryDict.remove character storyState.characterPlacements
    }


placeItem : item -> location -> StoryState item location character knowledge -> StoryState item location character knowledge
placeItem item location storyState =
    { storyState
        | itemPlacements =
            EveryDict.insert item (Prop location) storyState.itemPlacements
    }


addKnowledge : knowledge -> StoryState item location character knowledge -> StoryState item location character knowledge
addKnowledge knowledge storyState =
    if List.member knowledge storyState.knowledge then
        storyState
    else
        { storyState
            | knowledge = knowledge :: storyState.knowledge
        }


loadScene : List (Rule item location character knowledge) -> StoryState item location character knowledge -> StoryState item location character knowledge
loadScene scene storyState =
    { storyState
        | currentScene = loadCurrentScene scene
    }


endStory : StoryState item location character knowledge -> StoryState item location character knowledge
endStory storyState =
    storyState



-- Rules


findMatchingRule :
    RuleIndex
    -> Interactable item location character
    -> List (LiveRule item location character knowledge)
    -> StoryState item location character knowledge
    -> Maybe ( RuleIndex, LiveRule item location character knowledge )
findMatchingRule index interactable rules storyState =
    case rules of
        [] ->
            Nothing

        rule :: remainingRules ->
            if matchesRule rule interactable storyState then
                Just ( index, rule )
            else
                findMatchingRule (index + 1) interactable remainingRules storyState


matchesRule :
    LiveRule item location character knowledge
    -> Interactable item location character
    -> StoryState item location character knowledge
    -> Bool
matchesRule { interaction, conditions } interactable storyState =
    matchesInteraction interaction interactable
        && List.all (matchesCondition storyState) conditions


matchesInteraction :
    InteractionMatcher item location character
    -> Interactable item location character
    -> Bool
matchesInteraction interactionMatcher interactable =
    case interactionMatcher of
        WithAnything ->
            True

        WithAnyItem ->
            isItem interactable

        WithAnyLocation ->
            isLocation interactable

        WithAnyCharacter ->
            isCharacter interactable

        WithItem item ->
            Item item == interactable

        WithLocation location ->
            Location location == interactable

        WithCharacter character ->
            Character character == interactable


matchesCondition :
    StoryState item location character knowledge
    -> Condition item location character knowledge
    -> Bool
matchesCondition storyState condition =
    case condition of
        WithInventory item ->
            List.member item <| getInventory storyState

        NearCharacter character ->
            List.member character <| getCharactersInCurrentLocation storyState

        NearItem item ->
            List.member item <| getItemsInCurrentLocation storyState

        InLocation location ->
            storyState.currentLocation == location

        WithKnowledge knowledge ->
            List.member knowledge storyState.knowledge

        Unless condition ->
            Basics.not <| matchesCondition storyState condition
