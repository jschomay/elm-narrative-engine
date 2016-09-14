module Story.Element exposing (..)

import Color exposing (..)


type Element a b c
    = Item a
    | Location b
    | Character c


type alias Elements a b c =
    { items : a -> ItemInfo
    , locations : b -> LocationInfo
    , characters : c -> CharacterInfo
    }


type alias BasicInfo =
    { name : String
    , description : String
    }


type alias WithColor a =
    { a | color : Color }


type alias ItemInfo =
    BasicInfo


type alias LocationInfo =
    WithColor BasicInfo


type alias CharacterInfo =
    BasicInfo
