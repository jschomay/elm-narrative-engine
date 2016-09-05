module Scene2 exposing (..)

import StoryRules exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)

middle : Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
middle =
    [ given (InteractionWithItem Envelope) (Always)
        `changeWorld` []
        `narrate` "That went as well as could be expected.  Wonder where it came from?"
    ]

