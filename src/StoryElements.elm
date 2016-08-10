module StoryElements exposing (..)


type alias StoryElementsConfig element =
    element -> DisplayInformation


type alias DisplayInformation =
    { name : String
    , description : String
    }


getName : StoryElementsConfig a -> a -> String
getName displayConfig element =
    .name <| displayConfig element


getDescription : StoryElementsConfig a -> a -> String
getDescription displayConfig element =
    .description <| displayConfig element
