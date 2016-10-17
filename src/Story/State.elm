module Story.State exposing (..)

import Dict exposing (..)
import Types exposing (..)
import EveryDict exposing (..)


init : location -> List (Rule item location character knowledge) -> StoryState item location character knowledge
init startingLocation startingScene =
    StoryState startingLocation (loadCurrentScene startingScene) [ Location startingLocation ] [] [] EveryDict.empty EveryDict.empty []


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


advanceStory :
    String
    -> StoryState item location character knowledge
    -> List (ChangeWorldCommand item location character knowledge)
    -> String
    -> StoryState item location character knowledge
advanceStory displayableName storyState changesWorldCommands narration =
    let
        addNarration narration storyState =
            { storyState
                | storyLine = narration :: storyState.storyLine
            }

        doCommand command storyState =
            case command of
                MoveTo location ->
                    if List.member (Location location) storyState.familiarWith then
                        { storyState
                            | currentLocation = location
                        }
                    else
                        { storyState
                            | currentLocation = location
                            , familiarWith = storyState.familiarWith ++ [ Location location ]
                        }

                AddLocation location ->
                    if List.member location storyState.knownLocations then
                        storyState
                    else
                        { storyState
                            | knownLocations = location :: storyState.knownLocations
                        }

                RemoveLocation location ->
                    { storyState
                        | knownLocations = List.filter ((/=) location) storyState.knownLocations
                    }

                AddInventory item ->
                    { storyState
                        | itemPlacements =
                            EveryDict.insert item Inventory storyState.itemPlacements
                    }

                RemoveInventory item ->
                    { storyState
                        | itemPlacements = EveryDict.remove item storyState.itemPlacements
                    }

                MoveCharacter character location ->
                    { storyState
                        | characterPlacements =
                            EveryDict.insert character location storyState.characterPlacements
                    }

                RemoveCharacter character ->
                    { storyState
                        | characterPlacements = EveryDict.remove character storyState.characterPlacements
                    }

                PlaceItem item location ->
                    { storyState
                        | itemPlacements =
                            EveryDict.insert item (Prop location) storyState.itemPlacements
                    }

                RemoveItem item ->
                    { storyState
                        | itemPlacements = EveryDict.remove item storyState.itemPlacements
                    }

                AddKnowledge knowledge ->
                    if List.member knowledge storyState.knowledge then
                        storyState
                    else
                        { storyState
                            | knowledge = knowledge :: storyState.knowledge
                        }

                LoadScene scene ->
                    { storyState
                        | currentScene = loadCurrentScene scene
                    }

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( displayableName, narration )



-- Rules


findMatchingRule :
    RuleIndex
    -> Displayable item location character
    -> List (LiveRule item location character knowledge)
    -> StoryState item location character knowledge
    -> Maybe ( RuleIndex, LiveRule item location character knowledge )
findMatchingRule index displayable rules storyState =
    case rules of
        [] ->
            Nothing

        rule :: remainingRules ->
            if matchesRule rule displayable storyState then
                Just ( index, rule )
            else
                findMatchingRule (index + 1) displayable remainingRules storyState


matchesRule :
    LiveRule item location character knowledge
    -> Displayable item location character
    -> StoryState item location character knowledge
    -> Bool
matchesRule rule displayable storyState =
    rule.interaction == displayable && List.all (matchesCondition storyState) rule.conditions


matchesCondition :
    StoryState item location character knowledge
    -> Condition item location character knowledge
    -> Bool
matchesCondition storyState condition =
    case condition of
        WithItem item ->
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
