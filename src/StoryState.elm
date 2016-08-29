module StoryState exposing (..)

import Dict exposing (..)


type alias StoryState a b c d =
    { currentLocation : b
    , currentScene : d
    , inventory : List a
    , knownLocations : List b
    , storyLine : List ( String, String )
    , itemsByLocation : Dict String (List a)
    , charactersByLocation : Dict String (List c)
    }


init : b -> d -> StoryState a b c d
init startingLocation startingScene =
    StoryState startingLocation startingScene [] [] [] Dict.empty Dict.empty


getCharactersInCurrentLocation : StoryState a b c d -> List c
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getPropsInCurrentLocation : StoryState a b c d -> List a
getPropsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getCharactersByLocation : b -> StoryState a b c d -> List c
getCharactersByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.charactersByLocation


getItemsByLocation : b -> StoryState a b c d -> List a
getItemsByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.itemsByLocation


setCurrentLocation : b -> StoryState a b c d -> StoryState a b c d
setCurrentLocation location storyState =
    { storyState
        | currentLocation = location
    }


setCurrentScene : d -> StoryState a b c d -> StoryState a b c d
setCurrentScene scene storyState =
    { storyState
        | currentScene = scene
    }


addLocation : b -> StoryState a b c d -> StoryState a b c d
addLocation location storyState =
    if List.member location storyState.knownLocations then
        storyState
    else
        { storyState
            | knownLocations = location :: storyState.knownLocations
        }


removeLocation : b -> StoryState a b c d -> StoryState a b c d
removeLocation location storyState =
    { storyState
        | knownLocations = List.filter ((/=) location) storyState.knownLocations
    }


addInventory : a -> StoryState a b c d -> StoryState a b c d
addInventory item storyState =
    if List.member item storyState.inventory then
        storyState
    else
        { storyState
            | inventory = item :: storyState.inventory
        }


removeInventory : a -> StoryState a b c d -> StoryState a b c d
removeInventory item storyState =
    { storyState
        | inventory = List.filter ((/=) item) storyState.inventory
    }


addCharacter : c -> b -> StoryState a b c d -> StoryState a b c d
addCharacter character location storyState =
    if List.member character (getCharactersByLocation location storyState) then
        storyState
    else
        { storyState
            | charactersByLocation =
                Dict.insert (toString location)
                    ((getCharactersByLocation location storyState) ++ [ character ])
                    storyState.charactersByLocation
        }


removeCharacter : c -> b -> StoryState a b c d -> StoryState a b c d
removeCharacter character location storyState =
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


addProp : a -> b -> StoryState a b c d -> StoryState a b c d
addProp prop location storyState =
    if List.member prop (getItemsByLocation location storyState) then
        storyState
    else
        { storyState
            | itemsByLocation =
                Dict.insert (toString location)
                    ((getItemsByLocation location storyState) ++ [ prop ])
                    storyState.itemsByLocation
        }


removeProp : a -> b -> StoryState a b c d -> StoryState a b c d
removeProp prop location storyState =
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


addNarration : ( String, String ) -> StoryState a b c d -> StoryState a b c d
addNarration narration storyState =
    { storyState
        | storyLine = narration :: storyState.storyLine
    }
