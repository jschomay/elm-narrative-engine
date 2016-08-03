module Main exposing (..)

import Engine exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import Components.Storyline exposing (..)

-- import Engine exposing (loadStory)
-- import Engine.StaticElements exposing (Item, Location, Character)
-- import Engine.DynamicElements exposing (Narrative, Scene, StoryRule, ChangeWorldCommand(..), Condition(..))

-- main : Program Never


main =
    loadStory ...


type alias Name = String
type alias Description = String

type StoryElement a
    = StoryElement a Name Description

type alias Location a = StoryElement a
type alias Item a = StoryElement a
type alias Character a = StoryElement a


type alias Scene a b
    = { rules : List (StoryRule a b)
      , narration : List (Narration b)
      }

type Narration a
  = Narration a DisplayText


type DisplayText =
  Just String
  | InOrder (List String)

type StoryRule (StoryElement a) (Narration b)
    = StoryRule (Trigger a) (Condition a) List (ChangeWorldCommand a) b

type Trigger (StoryElement a)
    = Always
    | InteractWith (a)

type Condition a
    = Always
    | With (List a)
    | WithOut (List a)


type ChangeWorldCommand a
    = LoadScene (Scene a)
    | GoTo (Location a)
    | AddLocation (Location a)
    | RemoveLocation (Location a)
    | AddInventory (Item a)
    | RemoveInventory (Item a)
    | EnterCharacter (Character a)
    | ExitCharacter (Character a)
    | AddProp (Item a)
    | RemoveProp (Item a)


type alias Model =
    { currentScene : Scene a
    , currentLocation : Location a
    , storyWorld : List (StoryElement a)
    , inventory : List (Item a)
    , knownLocations : List (Location a)
    , stage :
        { characters : List (Character a)
        , props : List (Item a)
        }
    }


-- StoryRule (InteractWith Envelope) (With Podium) [] ReadSpeach
-- StoryRule (InteractWith MysteryMan) (WithOut Auditorium) [AddLocation Auditorium)

