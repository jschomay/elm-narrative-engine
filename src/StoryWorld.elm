module StoryWorld exposing (..)

import String


type StoryWorld element
    = StoryWorld (element -> DisplayInformation)


type alias DisplayInformation =
    { name: String
    , description: String
    }


getName : StoryWorld a -> a -> String
getName (StoryWorld display) element =
    .name <| display element


getDescription : StoryWorld a -> a -> String
getDescription (StoryWorld display) element =
    .description <| display element

