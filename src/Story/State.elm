module Story.State exposing (..)

import Dict exposing (..)
import Types exposing (..)


-- StoryState


init : location -> List (Rule item location character knowledge) -> StoryState item location character knowledge
init startingLocation startingScene =
    StoryState startingLocation startingScene [ Location startingLocation ] Dict.empty [] [] [] Dict.empty Dict.empty []


getCharactersInCurrentLocation : StoryState item location character knowledge -> List character
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getPropsInCurrentLocation : StoryState item location character knowledge -> List item
getPropsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getCharactersByLocation : location -> StoryState item location character knowledge -> List character
getCharactersByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.charactersByLocation


getItemsByLocation : location -> StoryState item location character knowledge -> List item
getItemsByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.itemsByLocation


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
                    if List.member item storyState.inventory then
                        storyState
                    else
                        { storyState
                            | inventory = item :: storyState.inventory
                        }

                RemoveInventory item ->
                    { storyState
                        | inventory = List.filter ((/=) item) storyState.inventory
                    }

                AddCharacter character location ->
                    if List.member character (getCharactersByLocation location storyState) then
                        storyState
                    else
                        { storyState
                            | charactersByLocation =
                                Dict.insert (toString location)
                                    ((getCharactersByLocation location storyState) ++ [ character ])
                                    storyState.charactersByLocation
                        }

                RemoveCharacter character location ->
                    if not <| Dict.member (toString location) storyState.charactersByLocation then
                        storyState
                    else
                        { storyState
                            | charactersByLocation =
                                Dict.insert (toString location)
                                    (getCharactersByLocation location storyState
                                        |> List.filter ((/=) character)
                                    )
                                    storyState.charactersByLocation
                        }

                AddProp prop location ->
                    if List.member prop (getItemsByLocation location storyState) then
                        storyState
                    else
                        { storyState
                            | itemsByLocation =
                                Dict.insert (toString location)
                                    ((getItemsByLocation location storyState) ++ [ prop ])
                                    storyState.itemsByLocation
                        }

                RemoveProp prop location ->
                    if not <| Dict.member (toString location) storyState.itemsByLocation then
                        storyState
                    else
                        { storyState
                            | itemsByLocation =
                                Dict.insert (toString location)
                                    (getItemsByLocation location storyState
                                        |> List.filter ((/=) prop)
                                    )
                                    storyState.itemsByLocation
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
                        | currentScene = scene
                        , matchedRules = Dict.empty
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
    -> List (Rule item location character knowledge)
    -> StoryState item location character knowledge
    -> Maybe ( RuleIndex, Rule item location character knowledge )
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
    Rule item location character knowledge
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
            List.member item storyState.inventory

        NearCharacter character ->
            List.member character <| getCharactersInCurrentLocation storyState

        NearProp prop ->
            List.member prop <| getPropsInCurrentLocation storyState

        InLocation location ->
            storyState.currentLocation == location

        WithKnowledge knowledge ->
            List.member knowledge storyState.knowledge

        Unless condition ->
            Basics.not <| matchesCondition storyState condition
