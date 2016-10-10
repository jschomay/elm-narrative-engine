module Types exposing (..)

import Dict
import Color


-- StoryState


type alias StoryState item location character knowledge =
    { currentLocation : location
    , currentScene : List (Rule item location character knowledge)
    , familiarWith : List (Displayable item location character)
    , matchedRules : Dict.Dict RuleIndex Int
    , inventory : List item
    , knownLocations : List location
    , storyLine : List ( String, String )
    , itemsByLocation : Dict.Dict String (List item)
    , charactersByLocation : Dict.Dict String (List character)
    , knowledge : List knowledge
    }


type alias RuleIndex =
    Int



-- Rules


type alias Rule item location character knowledge =
    { interaction : Displayable item location character
    , conditions : List (Condition item location character knowledge)
    , changes : List (ChangeWorldCommand item location character knowledge)
    , narration : List String
    }


type Condition item location character knowledge
    = WithItem item
    | NearCharacter character
    | NearProp item
    | InLocation location
    | WithKnowledge knowledge
    | Unless (Condition item location character knowledge)


type ChangeWorldCommand item location character knowledge
    = MoveTo location
    | AddLocation location
    | RemoveLocation location
    | AddInventory item
    | RemoveInventory item
    | AddCharacter character location
    | RemoveCharacter character location
    | AddProp item location
    | RemoveProp item location
    | AddKnowledge knowledge
    | LoadScene (List (Rule item location character knowledge))
    | EndStory



-- Displayables


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
    { a | color : Color.Color }


type alias ItemInfo =
    BasicInfo


type alias LocationInfo =
    WithColor BasicInfo


type alias CharacterInfo =
    BasicInfo
