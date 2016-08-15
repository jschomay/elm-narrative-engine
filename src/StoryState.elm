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


getCharactersByLocation : a -> Dict String (List a) -> Maybe (List a)
getCharactersByLocation location charactersByLocation =
    Dict.get (toString location) charactersByLocation


getItemsByLocation : a -> Dict String (List a) -> Maybe (List a)
getItemsByLocation location itemsByLocation =
    Dict.get (toString location) itemsByLocation


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
    { storyState
        | charactersByLocation =
            Dict.insert (toString location)
                (character
                    :: (Maybe.withDefault []
                            <| getCharactersByLocation location storyState.charactersByLocation
                       )
                )
                storyState.charactersByLocation
    }


removeCharacter : a -> a -> StoryState a b -> StoryState a b
removeCharacter character location storyState =
    { storyState
        | charactersByLocation =
            Dict.insert (toString location)
                (Maybe.withDefault []
                    <| getCharactersByLocation location storyState.charactersByLocation
                    `Maybe.andThen` (Just << List.filter ((/=) character))
                )
                storyState.charactersByLocation
    }


addProp : a -> a -> StoryState a b -> StoryState a b
addProp character prop storyState =
    { storyState
        | itemsByLocation =
            Dict.insert (toString prop)
                (character
                    :: (Maybe.withDefault []
                            <| getCharactersByLocation prop storyState.itemsByLocation
                       )
                )
                storyState.itemsByLocation
    }


removeProp : a -> a -> StoryState a b -> StoryState a b
removeProp character prop storyState =
    { storyState
        | itemsByLocation =
            Dict.insert (toString prop)
                (Maybe.withDefault []
                    <| getCharactersByLocation prop storyState.itemsByLocation
                    `Maybe.andThen` (Just << List.filter ((/=) character))
                )
                storyState.itemsByLocation
    }


addNarration : String -> StoryState a b -> StoryState a b
addNarration narration storyState =
    { storyState
        | storyLine = narration :: storyState.storyLine
    }
