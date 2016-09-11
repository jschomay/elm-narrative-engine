module Main exposing (..)

import Engine exposing (..)
import StoryRules exposing (..)
import StoryElements exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)
import Scene1 exposing (..)
import Scene2 exposing (..)


main : Program Never
main =
    loadStory storyInfo storySetup displayInfo scenes


storyInfo : StoryInfo
storyInfo =
    { title = "The curse of the tech demo"
    , byline = "Jeff Schomay"
    , prologue = prologue
    }


displayInfo : DisplayInfo MyItem MyLocation MyCharacter
displayInfo =
    { items = Items.displayInfo
    , locations = Locations.displayInfo
    , characters = Characters.displayInfo
    }


storySetup : StorySetup MyItem MyLocation MyCharacter MyScene MyKnowledge
storySetup =
    { startingScene = Scene1
    , startingLocation = Kitchen
    , startingNarration = startingNarration
    , storyWorldSetupCommands =
        [ AddInventory Envelope
        , AddLocation Kitchen
        , AddLocation Hallway
        , AddCharacter Moderator Auditorium
        , AddCharacter AnxiousMan Hallway
        , AddProp ExitSign Kitchen
        , AddProp Podium Auditorium
        ]
    }


scenes : MyScene -> Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
scenes scene =
    case scene of
        Scene1 ->
            scene1

        Scene2 ->
            scene2


prologue : String
prologue =
    """![](example/img/audience.jpg)

**Only two weeks left.**

You've been practicing hard, running through your presentation over and over, trying to remember all the pieces.  You want to be ready when the time comes.

You must have fallen asleep, because *something definitely doesn't seem right...*"""


startingNarration : String
startingNarration =
    """You find yourself standing in the middle of a dark, cavernous commercial kitchen.  The lights are all off, save for the glowing green emergency exit sign glinting off the polished stainless steel counter tops, and a shaft of warm light through the small windows of the double doors leading out to a hallway.

You stand there, butterflies in your stomach, but you can't remember how you got there, or even why you are there.  All you know is that you feel nervous of what awaits you just outside.
"""
