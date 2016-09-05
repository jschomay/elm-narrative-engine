module Main exposing (..)

import Color exposing (..)
import Engine exposing (..)
import Engine exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)


main : Program Never
main =
    loadStory "The curse of the tech demo" "Jeff Schomay" prologue storySetup storyItems storyLocations storyCharacters storyRules


prologue : String
prologue =
    """![](/img/audience.jpg)

**Only two weeks left.**

You've been practicing hard, running through your presentation over and over, trying to remember all the pieces.  You want to be ready when the time comes.

You must have fallen asleep, because *something definitely doesn't seem right...*"""


startingNarration : String
startingNarration =
    """You are in the commercial kitchen.  You don't know how you got here, or what you are doing here.

The only other person here is an anxious young man trying hard to get your attention."""


storySetup : StorySetup MyItem MyLocation MyCharacter MyScene
storySetup =
    { startingScene = Beginning
    , startingLocation = Kitchen
    , startingNarration = startingNarration
    , storyWorldSetupCommands =
        [ AddInventory Envelope
        , AddLocation Kitchen
        , AddCharacter Volunteer Kitchen
        , AddCharacter Moderator Kitchen
        , AddProp KitchenExit Kitchen
        , AddProp Podium Auditorium
        ]
    }


storyItems : ItemsInfo MyItem
storyItems tag =
    case tag of
        KitchenExit ->
            item "emergency exit" "The handle has a warning: \"Opening will sound the alarm\""

        Envelope ->
            item "thick envelope" "You found it crammed in your pocket, but you don't recoginze it."

        Podium ->
            item "podium" "The podium is on the stage, facing the audience.  Right in the spotlight."

        Mic ->
            item "microphone" "\"Testing one, two.\" Yup, it works fine."


storyLocations : LocationsInfo MyLocation
storyLocations tag =
    case tag of
        Kitchen ->
            location "commercial kitchen" blue "Rows of stainless steel counters fill the floor, with banks of ovens, stoves, and walk-in fridges along the walls."

        Auditorium ->
            location "auditorium" purple "The lights are low, the plush velvet seats filled to capacity.  A spotlight shines down on the podium on the stage."

        Hallway ->
            location "hallway" darkOrange "Just outside the main auditorium, where a few straglers wander by with hot drinks or converse in hushed debates."


storyCharacters : CharactersInfo MyCharacter
storyCharacters tag =
    case tag of
        Volunteer ->
            character "volunteer" "He sure seems stressed.  You're not helping."

        Moderator ->
            character "moderator" "She seems to command the audience and speakers with ease.  This definitely isn't her first rodeo."


storyRules : SceneSelector MyItem MyLocation MyCharacter MyScene
storyRules scene =
    case scene of
        Beginning ->
            beginning

        Middle ->
            middle


beginning : Scene MyItem MyLocation MyCharacter MyScene
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

![](/img/stage-fright.jpg)

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


middle : Scene MyItem MyLocation MyCharacter MyScene
middle =
    [ given (InteractionWithItem Envelope) (Always)
        `changeWorld` []
        `narrate` "That went as well as could be expected.  Wonder where it came from?"
    ]


strangerPreventsYouFromLeaving : String
strangerPreventsYouFromLeaving =
    "You think about escaping, but both the moderator and the anxious Volunteer glare at you as if to say *\"Don't even think about it!\"*"


type MyScene
    = Beginning
    | Middle


type MyItem
    = Envelope
    | Podium
    | Mic
    | KitchenExit


type MyLocation
    = Kitchen
    | Auditorium
    | Hallway


type MyCharacter
    = Volunteer
    | Moderator
