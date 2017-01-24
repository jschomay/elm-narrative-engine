module Types exposing (..)

import Dict exposing (Dict)


-- Model


type alias Story =
    { currentLocation : ID
    , currentScene : ID
    , history : List ID
    , manifest : Manifest
    , scenes : Scenes
    , theEnd : Maybe String
    }


type alias SceneName =
    String


type alias RuleName =
    String


type alias ID =
    String



-- Manifest


type alias Manifest =
    Dict ID Interactable


type alias Shown =
    Bool


type CharacterPlacement
    = CharacterInLocation ID
    | CharacterOffScreen


type ItemPlacement
    = ItemInLocation ID
    | ItemInInventory
    | ItemOffScreen


type alias Fixed =
    Bool


type Interactable
    = Item Fixed ItemPlacement
    | Location Shown
    | Character CharacterPlacement



-- Rules


type alias RuleIndex =
    Int


type alias Scenes =
    Dict ID Scene


type alias Scene =
    Dict ID Rule


type alias Rule =
    { interaction : InteractionMatcher
    , conditions : List Condition
    , changes : List ChangeWorldCommand
    }


type InteractionMatcher
    = WithAnything
    | WithAnyItem
    | WithAnyLocation
    | WithAnyCharacter
    | WithItem ID
    | WithLocation ID
    | WithCharacter ID


type Condition
    = ItemIsInInventory ID
    | CharacterIsInLocation ID ID
    | CharacterIsNotInLocation ID ID
    | CurrentLocationIs ID
    | CurrentLocationIsNot ID
    | ItemIsInLocation ID ID
    | ItemIsNotInInventory ID
    | ItemIsNotInLocation ID ID
    | BeenThereDoneThat ID
    | NotBeenThereDoneThat ID


type ChangeWorldCommand
    = MoveTo ID
    | AddLocation ID
    | RemoveLocation ID
    | MoveItemToLocationFixed ID ID
    | MoveItemToLocation ID ID
    | MoveItemToInventory ID
    | MoveItemOffScreen ID
    | MoveCharacterToLocation ID ID
    | MoveCharacterOffScreen ID
    | LoadScene String
    | EndStory String
