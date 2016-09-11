module Scene1 exposing (..)

import StoryRules exposing (..)
import StoryElements exposing (..)
import Items exposing (..)
import Locations exposing (..)
import Characters exposing (..)
import Knowledge exposing (..)
import Scenes exposing (..)


scene1 : Scene MyItem MyLocation MyCharacter MyScene MyKnowledge
scene1 =
    [ interactingWith (location Hallway)
        `when` (inLocation Kitchen)
        `changesWorld` []
        `narrates` afraidOfHallway
    , firstInteractionWith (item Envelope)
        `when` (everyTime)
        `changesWorld` [ addCharacter Volunteer Kitchen ]
        `narrates` volunteerArrives
    , firstInteractionWith (character Volunteer)
        `when` (everyTime)
        `changesWorld` [ addLocation Auditorium
                       , moveTo Auditorium
                       , removeCharacter Volunteer Kitchen
                       , addCharacter Volunteer Auditorium
                       ]
        `narrates` followVolunteer
    , interactingWith (location Hallway)
        `when` (unless (nearProp Mic))
        `changesWorld` []
        `narrates` tryToEscape
    , interactingWith (location Kitchen)
        `when` (all [ inLocation Auditorium, (unless (nearProp Mic)) ])
        `changesWorld` []
        `narrates` tryToEscape
    , interactingWith (item Podium)
        `when` (unless (nearProp Mic))
        `changesWorld` [ addProp Mic Auditorium ]
        `narrates` approachPodium
    , interactingWith (location Hallway)
        `when` (nearProp Mic)
        `changesWorld` []
        `narrates` thinkAboutEscaping
    , interactingWith (location Kitchen)
        `when` (nearProp Mic)
        `changesWorld` []
        `narrates` thinkAboutEscaping
    , interactingWith (item Envelope)
        `when` (nearProp Mic)
        `changesWorld` [ removeProp Mic Auditorium
                       , loadScene Scene2
                       ]
        `narrates` giveSpeach
    ]


afraidOfHallway : String
afraidOfHallway =
    """It's the only way out that you can see, but even thinking of going out there sends a shiver of nerves through your body, though you don't even know why."""


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
    """Ahh yes, the envelope in your pocket.  Now must be the time.

You pull out the folded sheets of paper and flip to the first one.  Let's see...

"Ahem.  'Mitochondrial transmembrane potential and Apoptosis.'"  Oh boy, what a mouthful.  Well, better give them what they want.

"Our study focused on measuring altered cellular oxidation-reductions in mitochondrial..."

Eyes begin glazing over.  That goes on for at least 20 minutes.  Finally you finish the last page.

"... in conclusion, the study failed to return a significant correlation to the hypothesis.  No questions?  Great, see ya'!"

Half the audience has fallen asleep.  You slink off stage.  At least that's done.
"""
