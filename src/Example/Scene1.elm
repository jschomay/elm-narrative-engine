module Scene1 exposing (..)

import StoryRules exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)

beginning : Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
beginning =
    [ given (InteractionWithItem KitchenExit) (Always)
        `changeWorld` []
        `narrate` """
A way out.  You head for the emergency exit, but the Volunteer stops you.

"You can't leave!  Everyone is waiting for you!"
"""
    , given (FirstInteractionWithCharacter Volunteer) (InLocation Kitchen)
        `changeWorld` [ AddLocation Auditorium ]
        `narrate` """
"Finally!  You drifted off for a minute there.  Come on, they are ready for you in auditorium.  Let's go."
"""
    , given (InteractionWithCharacter Volunteer) (InLocation Kitchen)
        `changeWorld` [ AddLocation Auditorium ]
        `narrate` """
"What are you waiting for, get out there!"
"""
    , given (InteractionWithLocation Auditorium) (InLocation Kitchen)
        `changeWorld` [ MoveTo Auditorium
                      , AddCharacter Volunteer Auditorium
                      , RemoveCharacter Volunteer Kitchen
                      , AddLocation Hallway
                      ]
        `narrate` """
You follow the Volunteer into the auditorium.  Stepping in, you see the large room, packed with eager audience members.

A woman at the podium addresses the audience.  "... and it looks like our next speaker has just arrived!  I'll hand it over to him."

She steps back, leading the audience in a welcoming applause, as they all turn and look, right at you.
"""
    , given (InteractionWithCharacter Moderator) (Always)
        `changeWorld` []
        `narrate` """
She smiles and nods at you politely, but her eyes say *"If you stall one more minute I'm going to wring your neck!*"
"""
    , given (InteractionWithItem Podium) (Not (WithItem Mic))
        `changeWorld` [ AddInventory Mic ]
        `narrate` """
You take a deep breath and make your way on stage.  The audience falls silent.  All eyes are on you.  Better say something quick.

![](example/img/stage-fright.jpg)

"Ahem.  Ladies, gentlemen, distinguished guests..."

Um... what now?
"""
    , given (InteractionWithLocation Hallway) (All [ NearProp Podium ])
        `changeWorld` []
        `narrate` strangerPreventsYouFromLeaving
    , given (InteractionWithLocation Kitchen) (All [ NearProp Podium ])
        `changeWorld` []
        `narrate` strangerPreventsYouFromLeaving
    , given (InteractionWithItem Envelope) (WithItem Mic)
        `changeWorld` [ RemoveInventory Mic, LoadScene Middle ]
        `narrate` """
Ahh yes, the envelope.  Now must be the time.

You pull out the folded sheets of paper and flip to the first one.

"Filliment mitochondriosus in nocturnal invertebrates."

Oh boy, this seems headdy.  Well, better give them what they want.

"When studying nicrofilates in laboratory conditions, we were able to detect minute levels of monochrio-deteriation..."

That goes on for at least 20 minutes.  Finally you finish the last page.

"... concluding our study, which returned an insignificant correlation to our hypothesis.

Any questions?  No.  Thank you very much, I'm off!"

The audience stares back, every eye in the room glazed over.  The moderator tries to recover, but doesn't seem to know what to say.  You slink off to the back of the room.  At least that's done.
"""
    ]



strangerPreventsYouFromLeaving : String
strangerPreventsYouFromLeaving =
    "You think about escaping, but both the moderator and the anxious Volunteer glare at you as if to say *\"Don't even think about it!\"*"




