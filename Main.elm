module Main exposing (..)

import Story exposing (..)
import Color


main : Program Never
main =
    Story.load info interactables setup


info : Info
info =
    { title = "LOCAL TESTING"
    , byline = "Jeff"
    , prologue = "Just a simple story demo to test the engine with."
    }


interactables : StoryWorld MyItem MyLocation MyCharacter
interactables =
    storyWorld items locations characters


setup : Setup MyItem MyLocation MyCharacter MyKnowledge
setup =
    { startingScene = scene1
    , startingLocation = Home
    , startingNarration = "Begin..."
    , setupCommands =
        [ addInventory Umbrella
        , placeItem Marble Marsh
        , addLocation Home
        , addLocation Garden
        , moveCharacter Harry Garden
        ]
    }


type MyItem
    = Umbrella
    | Marble


type MyLocation
    = Home
    | Garden
    | Marsh


type MyCharacter
    = Harry


type MyKnowledge
    = NA


items : MyItem -> ItemInfo
items i =
    case i of
        Umbrella ->
            itemInfo "Umbrella" "My umbrella..."

        Marble ->
            itemInfo "Marble" "Oooh, shiny.."


characters : MyCharacter -> CharacterInfo
characters c =
    case c of
        Harry ->
            characterInfo "Harry" "About 5 foot tall, red hair..."


locations : MyLocation -> LocationInfo
locations l =
    case l of
        Home ->
            locationInfo "Home" Color.red "No place like home..."

        Garden ->
            locationInfo "Garden" Color.red "What a lovely garden..."

        Marsh ->
            locationInfo "Marsh" Color.red "Nice and green..."


scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
scene1 =
    [ { interaction = withItem Umbrella
      , conditions = []
      , changes = []
      , narration =
            [ "It really is a nice umbrealla."
            , "It's not raining now."
            , "Don't really need it."
            ]
      }
    , { interaction = withLocation Garden
      , conditions = []
      , changes = [ moveTo Garden, loadScene scene2 ]
      , narration = []
      }
    ]


scene2 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
scene2 =
    [ { interaction = withCharacter Harry
      , conditions = [ inLocation Garden ]
      , changes = [ moveCharacter Harry Marsh, moveTo Marsh ]
      , narration = [ "Come with me to the marsh" ]
      }
    , { interaction = withItem Marble
      , conditions = [ unless (withInventory Marble) ]
      , changes = [ addInventory Marble ]
      , narration = [ "Pretty!  Mine." ]
      }
    , { interaction = withItem Marble
      , conditions = []
      , changes = [ loadScene scene3 ]
      , narration = [ "I should take this home." ]
      }
    , { interaction = withItem Umbrella
      , conditions = [ inLocation Marsh ]
      , changes = []
      , narration = [ "Sure am glad I brought it with me!" ]
      }
    , { interaction = withCharacter Harry
      , conditions = [ inLocation Marsh ]
      , changes = []
      , narration =
            [ "My good friend Harry..."
            , "I've known him since we were young"
            , "Looks like rain"
            , """He seems out of things to say.

I'll keep trying anyways."""
            , "Nope, he really is out of things to say!"
            ]
      }
    ]


scene3 =
    [ { interaction = withLocation Home
      , conditions = [ withInventory Marble ]
      , changes = [ moveTo Home, placeItem Marble Home ]
      , narration =
            [ "This will be safe here." ]
      }
    , { interaction = withAnything
      , conditions = [ inLocation Home ]
      , changes = []
      , narration =
            [ "That's enough adventuring for today" ]
      }
    , { interaction = withAnyItem
      , conditions = []
      , changes = []
      , narration =
            [ "I don't want to mess with that, I need to get home." ]
      }
    , { interaction = withAnyLocation
      , conditions = []
      , changes = []
      , narration =
            [ "I'm only concerned with getting home." ]
      }
    , { interaction = withAnyCharacter
      , conditions = []
      , changes = []
      , narration =
            [ "No time to talk, I need to get home." ]
      }
    ]
