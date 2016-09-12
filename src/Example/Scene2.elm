module Scene2 exposing (..)

import StoryRules exposing (..)
import StoryState exposing (..)
import StoryElements exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)


scene2 : Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
scene2 =
    [ interactingWith (item Podium)
        `when` (everyTime)
        `changesWorld` []
        `narrates` doneWithPodium
    , interactingWith (character Moderator)
        `when` (everyTime)
        `changesWorld` []
        `narrates` moderatorAfterSpeech
    , firstInteractionWith (character AnxiousMan)
        `when` (everyTime)
        `changesWorld` [ addKnowledge LostNotes ]
        `narrates` lostSpeech
    , interactingWith (item Envelope)
        `when` (all
                    [ withKnowledge LostNotes
                    , nearCharacter AnxiousMan
                    ]
               )
        `changesWorld` [ removeInventory Envelope
                       , addInventory ElmSticker
                       , removeCharacter AnxiousMan Hallway
                       , endStory
                       ]
        `narrates` returningNotes
    , interactingWith (item Envelope)
        `when` (everyTime)
        `changesWorld` []
        `narrates` mysteriousSpeechNotes
    ]


doneWithPodium : String
doneWithPodium =
    "There's no need to get back up there.  Now to figure out how to get out of here."


moderatorAfterSpeech : String
moderatorAfterSpeech =
    """She tries to get things back on track.  She seems even less pleased now, after that little performance.
"""


mysteriousSpeechNotes : String
mysteriousSpeechNotes =
    """How the heck did you end up with those?  You didn't understand a word of what you said.
"""


lostSpeech : String
lostSpeech =
    """He looks even more anxious than you, pacing back and forth, sweating, and muttering to himself.

"Don't worry," you say, "it's not that bad."

"You don't understand!  My speech, my notes -- I've lost them!  I'm supposed to present in ten minutes!  What am I going to do?"

He goes back to muttering words of doom and gloom.  Poor guy.
"""


returningNotes : String
returningNotes =
    """"Oh you mean these?  You're welcome to them."

You hand over the envelope.  "Yes, that's it!  You found them!  You've saved me, how can I ever repay you?"

"Don't bother, just... good luck."

Well that wraps that up in a nice little package.  But it's still a mystery why you are here.

The old man calls you back.  "Oh, hey!  I think you should have this."

He reaches into his pocket and hands you something.  A sticker.

![](example/img/elm-sticker.png)

Wait a minute... that looks familiar.  Of course, Elm!  It's all coming back to you now.  This is ElmConf.  You're here to give your presentation.  A presentation called:

###Building an Interactive Storytelling Framework in Elm##

<span style="text-align: center; display: block;">_~ The end ~_</span>
"""
