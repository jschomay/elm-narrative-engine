module Story.Displayable exposing (..)

import Color exposing (..)


type Displayable item location character
    = Item item
    | Location location
    | Character character


type alias StoryWorld item location character =
    { items : item -> ItemInfo
    , locations : location -> LocationInfo
    , characters : character -> CharacterInfo
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
