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
        , addCharacter Harry Garden
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
            characterInfo "Harry" "My good friend Harry..."


locations : MyLocation -> LocationInfo
locations l =
    case l of
        Home ->
            locationInfo "Home" Color.red "No place like home..."

        Garden ->
            locationInfo "Garden" Color.red "What a lovely garden..."

        Marsh ->
            locationInfo "Marsh" Color.red "Hmm, looks like rain..."


scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
scene1 =
    [ { interaction = character Harry
      , conditions = [ inLocation Garden ]
      , changes = [ addCharacter Harry Marsh, removeCharacter Harry Garden ]
      , narration = "Meet me in the marsh..."
      }
    , { interaction = character Harry
      , conditions = [ inLocation Marsh ]
      , changes = []
      , narration = "My good friend Harry..."
      }
    ]
