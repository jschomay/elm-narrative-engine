module StoryElements exposing (..)

import Color exposing (..)


type StoryElement a b c
    = Item a
    | Location b
    | Character c


type alias BasicInfo =
    { name : String
    , description : String
    }


type alias WithColor a =
    { a | color : Color }


type alias ItemsInfo a =
    a -> BasicInfo


type alias LocationsInfo a =
    a -> WithColor BasicInfo


type alias CharactersInfo a =
    a -> BasicInfo


item : a -> StoryElement a b c
item =
    Item


location : b -> StoryElement a b c
location =
    Location


character : c -> StoryElement a b c
character =
    Character


getName : { a | name : String } -> String
getName { name } =
    name


getDescription : { a | description : String } -> String
getDescription { description } =
    description


getColor : WithColor a -> Color
getColor { color } =
    color


itemInfo : String -> String -> BasicInfo
itemInfo name description =
    { name = name
    , description = description
    }


locationInfo : String -> Color -> String -> WithColor BasicInfo
locationInfo name color description =
    { name = name
    , description = description
    , color = color
    }


characterInfo : String -> String -> BasicInfo
characterInfo name description =
    { name = name
    , description = description
    }
