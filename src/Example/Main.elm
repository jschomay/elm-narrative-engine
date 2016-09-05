module Main exposing (..)

import Engine exposing (..)
import StoryRules exposing (..)

import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)
import Scene1 exposing (..)
import Scene2 exposing (..)

main : Program Never
main =
    loadStory "The curse of the tech demo" "Jeff Schomay" prologue storySetup storyItems storyLocations storyCharacters storyRules


prologue : String
prologue =
    """![](example/img/audience.jpg)

**Only two weeks left.**

You've been practicing hard, running through your presentation over and over, trying to remember all the pieces.  You want to be ready when the time comes.

You must have fallen asleep, because *something definitely doesn't seem right...*"""


startingNarration : String
startingNarration =
    """You are in the commercial kitchen.  You don't know how you got here, or what you are doing here.

The only other person here is an anxious young man trying hard to get your attention."""


storySetup : StorySetup MyItem MyLocation MyCharacter MyScene MyKnowledge
storySetup =
    { startingScene = Beginning
    , startingLocation = Kitchen
    , startingNarration = startingNarration
    , storyWorldSetupCommands =
        [ AddInventory Envelope
        , AddLocation Kitchen
        , AddCharacter Volunteer Kitchen
        , AddCharacter Moderator Auditorium
        , AddProp KitchenExit Kitchen
        , AddProp Podium Auditorium
        ]
    }


storyRules : SceneSelector MyItem MyLocation MyCharacter MyScene MyKnowledge
storyRules scene =
    case scene of
        Beginning ->
            beginning

        Middle ->
            middle



