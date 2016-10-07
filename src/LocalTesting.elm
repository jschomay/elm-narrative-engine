module Main exposing (..)

{- This file does not get checked in, it is just to test developement locally -}

import Story exposing (..)
import Story.Rule exposing (..)
import Color


main : Program Never
main =
    Story.load info elements scenes setup


info : Info
info =
    { title = "LOCAL TESTING"
    , byline = "Jeff Schomay"
    , prologue = "This file does not get checked in, it is just to test developement locally"
    }


elements : Elements MyItem MyLocation MyCharacter
elements =
    storyWorld items locations characters


setup : Setup MyItem MyLocation MyCharacter MyScene MyKnowledge
setup =
    { startingScene = Scene1
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


scenes : MyScene -> Story.Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
scenes scene =
    case scene of
        Scene1 ->
            scene1

        Scenes2 ->
            scene1


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


type MyScene
    = Scene1
    | Scenes2


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


scene1 : List (Story.Rule.Rule f MyLocation MyCharacter g h)
scene1 =
    [ interactingWith (character Harry)
        `when` inLocation Garden
        `changesWorld` [ addCharacter Harry Marsh, removeCharacter Harry Garden ]
        `narrates` "Meet me in the marsh..."
    , interactingWith (character Harry)
        `when` inLocation Marsh
        `changesWorld` [ loadScene Scene2 ]
        `narrates` "Meet me in the marsh..."
    ]


scene2 : List (Story.Rule.Rule f MyLocation MyCharacter g h)
scene2 =
    [ interactingWith (character Harry)
        `when` inLocation Garden
        `changesWorld` []
        `narrates` "We've been here before!..."
    ]


scene2asRecord : List (Story.Rule.Rule f MyLocation MyCharacter g h)
scene2asRecord =
    [ { interaction = with (character Harry)
      , conditions = [ inLocation Garden ]
      , changes = []
      , narration = sayRepeating [ "We've been here before!...", "..." ]
      }
    , { interaction = with (character Harry)
      , conditions = [ inLocation Garden ]
      , changes = []
      , narration = say [ "We've been here before!..." ]
      }
    ]



-- scene2withPipes : List (Story.Rule.Rule f MyLocation MyCharacter g h)
-- scene2withPipes =
--     [ interactionWith (character Harry) say "..."
--       |> inLocation Garden
--       |> inLocation Home
--       |> removeCharacter Harry Garden
--       |> addCharacter Harry Marsh
--     ]
{-
