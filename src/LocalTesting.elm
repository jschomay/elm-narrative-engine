module Main exposing (..)

import Story exposing (..)
import Color


main : Program Never
main =
    Story.load info displayables setup


info : Info
info =
    { title = "LOCAL TESTING"
    , byline = "Jeff"
    , prologue = "Just a simple story demo to test the engine with."
    }


displayables : StoryWorld MyItem MyLocation MyCharacter
displayables =
    storyWorld items locations characters


setup : Setup MyItem MyLocation MyCharacter MyKnowledge
setup =
    { startingScene = scene1
    , startingLocation = Home
    , startingNarration = "Begin..."
    , setupCommands =
        [ addInventory Umbrella
        , addLocation Home
        , addLocation Garden
        , addLocation Marsh
        , addCharacter Harry Marsh
        ]
    }


type MyItem
    = Umbrella


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
    [ { interaction = item Umbrella
      , conditions = []
      , changes = []
      , narration =
            [ "It really is a nice umbrealla."
            , "It's not raining now."
            , "Don't really need it."
            ]
      }
    , { interaction = location Garden
      , conditions = []
      , changes = [ moveTo Garden, loadScene scene2 ]
      , narration = []
      }
    ]


scene2 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
scene2 =
    [ { interaction = character Harry
      , conditions = [ inLocation Garden ]
      , changes = [ addCharacter Harry Marsh, removeCharacter Harry Garden ]
      , narration = [ "Meet me in the marsh..." ]
      }
    , { interaction = item Umbrella
      , conditions = [ inLocation Marsh ]
      , changes = []
      , narration = [ "Sure am glad I brought it with me!" ]
      }
    , { interaction = character Harry
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
