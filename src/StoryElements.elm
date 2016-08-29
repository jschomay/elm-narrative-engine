module StoryElements exposing (..)


type StoryElement a b c
    = Item a
    | Location b
    | Character c


type alias BasicInfo =
    { name : String
    , description : String
    }


type alias ItemsInfo a =
    a -> BasicInfo


type alias LocationsInfo a =
    a -> BasicInfo


type alias CharactersInfo a =
    a -> BasicInfo


getName : { a | name : String } -> String
getName { name } =
    name


getDescription : { a | description : String } -> String
getDescription { description } =
    description


item : String -> String -> BasicInfo
item name description =
    { name = name
    , description = description
    }


location : String -> String -> BasicInfo
location name description =
    { name = name
    , description = description
    }


character : String -> String -> BasicInfo
character name description =
    { name = name
    , description = description
    }
