module StoryState exposing (..)

import Dict exposing (..)


type alias StoryState a =
    { currentLocation : a
    , inventory : List a
    , knownLocations : List a
    , storyLine : List String
    , itemsByLocation : Dict String (List a)
    , charactersByLocation : Dict String (List a)
    }


getCharactersByLocation : Dict String (List a) -> a -> Maybe (List a)
getCharactersByLocation charactersByLocation location =
    Dict.get (toString location) charactersByLocation


getItemsByLocation : Dict String (List a) -> a -> Maybe (List a)
getItemsByLocation itemsByLocation location =
    Dict.get (toString location) itemsByLocation
