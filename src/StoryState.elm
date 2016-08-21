module StoryState exposing (..)

import Dict exposing (..)


type alias StoryState a b =
    { currentLocation : a
    , currentScene : b
    , inventory : List a
    , knownLocations : List a
    , storyLine : List String
    , itemsByLocation : Dict String (List a)
    , charactersByLocation : Dict String (List a)
    }


init : a -> b -> StoryState a b
init startingLocation startingScene =
    StoryState startingLocation startingScene [] [] [] Dict.empty Dict.empty


getCharactersInCurrentLocation : StoryState a b -> List a
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getPropsInCurrentLocation : StoryState a b -> List a
getPropsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getCharactersByLocation : a -> StoryState a b -> List a
getCharactersByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.charactersByLocation


getItemsByLocation : a -> StoryState a b -> List a
getItemsByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.itemsByLocation


setCurrentLocation : a -> StoryState a b -> StoryState a b
setCurrentLocation location storyState =
    { storyState
        | currentLocation = location
    }


setCurrentScene : b -> StoryState a b -> StoryState a b
setCurrentScene scene storyState =
    { storyState
        | currentScene = scene
    }


addLocation : a -> StoryState a b -> StoryState a b
addLocation location storyState =
    if List.member location storyState.knownLocations then
        storyState
    else
        { storyState
            | knownLocations = location :: storyState.knownLocations
        }


removeLocation : a -> StoryState a b -> StoryState a b
removeLocation location storyState =
    { storyState
        | knownLocations = List.filter ((/=) location) storyState.knownLocations
    }


addInventory : a -> StoryState a b -> StoryState a b
addInventory item storyState =
    if List.member item storyState.inventory then
        storyState
    else
        { storyState
            | inventory = item :: storyState.inventory
        }


removeInventory : a -> StoryState a b -> StoryState a b
removeInventory item storyState =
    { storyState
        | inventory = List.filter ((/=) item) storyState.inventory
    }


addCharacter : a -> a -> StoryState a b -> StoryState a b
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


removeCharacter : a -> a -> StoryState a b -> StoryState a b
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


addProp : a -> a -> StoryState a b -> StoryState a b
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


removeProp : a -> a -> StoryState a b -> StoryState a b
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


addNarration : String -> StoryState a b -> StoryState a b
addNarration narration storyState =
    { storyState
        | storyLine = narration :: storyState.storyLine
    }
