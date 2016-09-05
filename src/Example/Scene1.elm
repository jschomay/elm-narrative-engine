module Scene1 exposing (..)

import StoryRules exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)


scene1 : Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
scene1 =
    [ given (InteractionWithLocation Hallway) (InLocation Kitchen)
        `changeWorld` []
        `narrate` afraidOfHallway
    , given (FirstInteractionWithItem Envelope) (Always)
        `changeWorld` [ AddCharacter Volunteer Kitchen ]
        `narrate` volunteerArrives
    , given (FirstInteractionWithCharacter Volunteer) (Always)
        `changeWorld` [ AddLocation Auditorium
                      , MoveTo Auditorium
                      , RemoveCharacter Volunteer Kitchen
                      , AddCharacter Volunteer Auditorium
                      ]
        `narrate` followVolunteer
    , given (InteractionWithLocation Hallway) (Not(NearProp Mic))
        `changeWorld` []
        `narrate` tryToEscape
    , given (InteractionWithLocation Kitchen) (Not(NearProp Mic))
        `changeWorld` []
        `narrate` tryToEscape
    , given (InteractionWithItem Podium) (Not (NearProp Mic))
        `changeWorld` [ AddProp Mic Auditorium ]
        `narrate` approachPodium
    , given (InteractionWithLocation Hallway) (NearProp Mic)
        `changeWorld` []
        `narrate` thinkAboutEscaping
    , given (InteractionWithLocation Kitchen) (NearProp Mic)
        `changeWorld` []
        `narrate` thinkAboutEscaping
    , given (InteractionWithItem Envelope) (NearProp Mic)
        `changeWorld` [ RemoveProp Mic Auditorium
                      , LoadScene Scene2
                      ]
        `narrate` giveSpeach
    ]


afraidOfHallway : String
afraidOfHallway =
    """It's the only way out you that you can see, but even thinking of going out there sends a shiver of nerves through your body, though you don't even know why."""


volunteerArrives : String
volunteerArrives =
    """You find an envelope in your pocket.  You don't recognize it.  It has a packet of thickly folded papers inside, but in this dim light you can't quite read them.  What is it doing in your pocket?

Just then, the double doors burst open and someone approaches.  You can just make out white letters on his shirt spelling "Volunteer."

"There you are!  I've been looking all over for you.  They're ready for you. Come on, follow me."
"""


followVolunteer : String
followVolunteer =
    """"Who's waiting for me?"

"Don't be silly, let's go." He ushers you through the double doors and down the hall to "Auditorium A."

From the back of the room you can the moderator at the podium on the stage, addressing a full audience.  She seems to be stalling for time.

"... and I see our next speaker has just arrived!  Without further ado, I'll turn microphone over."  She steps aside and the whole room turns around to look back at you.

_"Get up there!"_ the volunteer hisses through a forced smile.
"""


tryToEscape : String
tryToEscape =
    """Well this is terrifying.  Maybe you can just slip out the back--

"Oh no you don't!"  The volunteer snatches your arm and gives a you little shove towards the front of the room.
"""


approachPodium : String
approachPodium =
    """Looks like there's no avoiding it.  You timidly make your way down the aisle.

Up on stage you turn to face the eager audience.  The microphone stares you right in the face.

![](example/img/stage-fright.jpg)

You lean forward, "Um...  Hello... Ladies, gentlemen, distinguished guests..."

What now?
"""


thinkAboutEscaping : String
thinkAboutEscaping =
    "All eyes are on you, there's no way you can leave now."


giveSpeach : String
giveSpeach =
    """ Ahh yes, the envelope in your pocket.  Now must be the time.

You pull out the folded sheets of paper and flip to the first one.  Let's see...

"Ahem.  'Mitochondrial transmembrane potential and Apoptosis.'"  Oh boy, this seems heavy.  Well, better give them what they want.

"A variety of key events in apoptosis focus on mitochondria, including the release of caspase activators such as cytochrome c, altered cellular oxidation-reductions..."

What a mouthful.  That goes on for at least 20 minutes.  Finally you finish the last page.

"... in conclusion, the study failed to return a significant correlation to the hypothesis.  Any questions?  No?  Great, see ya'!"

Every eye in the room has glazed over.  You slink off stage.  At least that's done.
"""
