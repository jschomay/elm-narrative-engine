module Types exposing (..)

import Dict exposing (Dict)
import List.Zipper exposing (Zipper)


-- Msg


type Msg
    = NoOp
    | Interact ID



-- Model


type alias Story =
    { currentLocation : ID
    , currentScene : ID
    , history : List ID
    , manifest : Manifest
    , scenes : Scenes
    , theEnd : Maybe String
    , storyLine : List Narration
    }


type alias Narration =
    ( SceneName, Maybe RuleName, Maybe Attributes, Maybe String )


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


type alias Attributes =
    { name : String, description : String }


type CharacterPlacement
    = CharacterInLocation ID
    | CharacterOffScreen


type ItemPlacement
    = ItemInLocation ID
    | ItemInInventory
    | ItemOffScreen



-- TODO - make Attributes genaric if possible


type Interactable
    = Item ItemPlacement Attributes
    | Location Shown Attributes
    | Character CharacterPlacement Attributes



-- Rules


type alias RuleIndex =
    Int


type alias Scenes =
    Dict ID Scene


type alias Scene =
    Dict ID LiveRule


type alias Rule =
    { interaction : InteractionMatcher
    , conditions : List Condition
    , changes : List ChangeWorldCommand
    , narration : List String
    }


type alias LiveRule =
    { interaction : InteractionMatcher
    , conditions : List Condition
    , changes : List ChangeWorldCommand
    , narration : Zipper (Maybe String)
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
    | CharacterIsPresent ID
    | ItemIsPresent ID
    | IsInLocation ID
    | ItemIsNotInInventory ID
    | CharacterIsNotPresent ID
    | ItemIsNotPresent ID
    | IsNotInLocation ID


type ChangeWorldCommand
    = MoveTo ID
    | AddLocation ID
    | RemoveLocation ID
    | MoveItem ID ID
    | MoveItemToInventory ID
    | MoveItemOffScreen ID
    | MoveCharacter ID ID
    | MoveCharacterOffScreen ID
    | LoadScene String
    | EndStory String
