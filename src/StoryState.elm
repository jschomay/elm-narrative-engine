module StoryState exposing (..)

import Dict exposing (..)
import StoryElements exposing (..)


type alias StoryState a b c d e =
    { currentLocation : b
    , currentScene : d
    , familiarWith : List (StoryElement a b c)
    , inventory : List a
    , knownLocations : List b
    , storyLine : List ( String, String )
    , itemsByLocation : Dict String (List a)
    , charactersByLocation : Dict String (List c)
    , knowledge : List e
    }


type alias AdvanceStory a b c d e =
    ( ChangeWorldCommands a b c d e, Narration )


type alias ChangeWorldCommands a b c d e =
    List (ChangeWorldCommand a b c d e)


type ChangeWorldCommand a b c d e
    = MoveTo b
    | AddLocation b
    | RemoveLocation b
    | AddInventory a
    | RemoveInventory a
    | AddCharacter c b
    | RemoveCharacter c b
    | AddProp a b
    | RemoveProp a b
    | AddKnowledge e
    | LoadScene d
    | EndStory


type Narration
    = Narrate String


init : b -> d -> StoryState a b c d e
init startingLocation startingScene =
    StoryState startingLocation startingScene [ Location startingLocation ] [] [] [] Dict.empty Dict.empty []


moveTo : b -> ChangeWorldCommand a b c d e
moveTo =
    MoveTo


addLocation : b -> ChangeWorldCommand a b c d e
addLocation =
    AddLocation


removeLocation : b -> ChangeWorldCommand a b c d e
removeLocation =
    RemoveLocation


addInventory : a -> ChangeWorldCommand a b c d e
addInventory =
    AddInventory


removeInventory : a -> ChangeWorldCommand a b c d e
removeInventory =
    RemoveInventory


addCharacter : c -> b -> ChangeWorldCommand a b c d e
addCharacter =
    AddCharacter


removeCharacter : c -> b -> ChangeWorldCommand a b c d e
removeCharacter =
    RemoveCharacter


addProp : a -> b -> ChangeWorldCommand a b c d e
addProp =
    AddProp


removeProp : a -> b -> ChangeWorldCommand a b c d e
removeProp =
    RemoveProp


addKnowledge : e -> ChangeWorldCommand a b c d e
addKnowledge =
    AddKnowledge


loadScene : d -> ChangeWorldCommand a b c d e
loadScene =
    LoadScene


endStory : ChangeWorldCommand a b c d e
endStory =
    EndStory


getCharactersInCurrentLocation : StoryState a b c d e -> List c
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getPropsInCurrentLocation : StoryState a b c d e -> List a
getPropsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getCharactersByLocation : b -> StoryState a b c d e -> List c
getCharactersByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.charactersByLocation


getItemsByLocation : b -> StoryState a b c d e -> List a
getItemsByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.itemsByLocation


advanceStory : String -> StoryState a b c d e -> AdvanceStory a b c d e -> StoryState a b c d e
advanceStory storyElementName storyState ( changesWorldCommands, narration ) =
    let
        getNarration narration =
            case narration of
                Narrate t ->
                    t

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
                    }

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( storyElementName, getNarration narration )
