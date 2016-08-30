module StoryElements exposing (..)

import Color exposing (..)
import String exposing (join)


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


getName : { a | name : String } -> String
getName { name } =
    name


getDescription : { a | description : String } -> String
getDescription { description } =
    description


getColor : WithColor a -> Color
getColor { color } =
    color


toCssColor : Color -> String
toCssColor =
    toRgb >> \{ red, green, blue } -> String.join "" [ "rgb(", toString red, ",", toString green, ",", toString blue, ")" ]


item : String -> String -> BasicInfo
item name description =
    { name = name
    , description = description
    }


location : String -> Color -> String -> WithColor BasicInfo
location name color description =
    { name = name
    , description = description
    , color = color
    }


character : String -> String -> BasicInfo
character name description =
    { name = name
    , description = description
    }
