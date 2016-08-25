module Main exposing (..)

import Dict exposing (..)
import Engine exposing (..)
import Engine exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


main : Program Never
main =
    loadStory "Stage Fright" storyElements storyRules initialStoryState


initialStoryState : StoryState MyStoryElement MyScene
initialStoryState =
    { currentLocation = Kitchen
    , currentScene = Beginning
    , inventory = [ Envelope ]
    , knownLocations = [ Kitchen ]
    , storyLine = [ ( Kitchen, """
You are in the commercial kitchen.  You don't know how you got here, or what you are doing here.

The only other person here is an anxious young man trying hard to get your attention.
    """ ) ]
    , itemsByLocation = Dict.singleton (toString Auditorium) [ Podium ] |> Dict.insert (toString Kitchen) [ KitchenExit ]
    , charactersByLocation = Dict.singleton (toString Kitchen) [ Volunteer ] |> Dict.insert (toString Auditorium) [ Moderator ]
    }


storyElements : StoryElementsConfig MyStoryElement
storyElements element =
    case element of
        Volunteer ->
            DisplayInformation "volunteer" "He sure seems stressed.  You're not helping."

        Moderator ->
            DisplayInformation "moderator" "She seems to command the audience and speakers with ease.  This definitely isn't her first rodeo."

        Kitchen ->
            DisplayInformation "commercial kitchen" "Rows of stainless steel counters fill the floor, with banks of ovens, stoves, and walk-in fridges along the walls."

        Auditorium ->
            DisplayInformation "auditorium" "The lights are low, the plush velvet seats filled to capacity.  A spotlight shines down on the podium on the stage."

        Hallway ->
            DisplayInformation "hallway" "Just outside the main auditorium, where a few straglers wander by with hot drinks or converse in hushed debates."

        KitchenExit ->
            DisplayInformation "emergency exit" "The handle has a warning: \"Opening will sound the alarm\""

        Envelope ->
            DisplayInformation "thick envelope" "You found it crammed in your pocket, but you don't recoginze it."

        Podium ->
            DisplayInformation "podium" "The podium is on the stage, facing the audience.  Right in the spotlight."

        Mic ->
            DisplayInformation "microphone" "\"Testing one, two.\" Yup, it works fine."


storyRules : StoryRulesConfig MyStoryElement MyScene
storyRules scene =
    case scene of
        Beginning ->
            beginning

        Middle ->
            middle


beginning : Scene MyStoryElement MyScene
beginning =
    [ given (InteractionWith KitchenExit) (Always)
        `do` []
        `narrate` Simple """
A way out.  You head for the emergency exit, but the Volunteer stops you.

"You can't leave!  Everyone is waiting for you!"
"""
    , given (FirstInteractionWith Volunteer) (InLocation Kitchen)
        `do` [ AddLocation Auditorium ]
        `narrate` Simple """
"Finally!  You drifted off for a minute there.  Come on, they are ready for you in auditorium.  Let's go."
"""
    , given (InteractionWith Volunteer) (InLocation Kitchen)
        `do` [ AddLocation Auditorium ]
        `narrate` Simple """
"What are you waiting for, get out there!"
"""
    , given (InteractionWith Auditorium) (InLocation Kitchen)
        `do` [ MoveTo Auditorium, AddCharacter Volunteer Auditorium, AddLocation Hallway ]
        `narrate` Simple """
You follow the Volunteer into the auditorium.  Stepping in, you see the large room, packed with eager audience members.

A woman at the podium addresses the audience.  "... and it looks like our next speaker has just arrived!  I'll hand it over to him."

She steps back, leading the audience in a welcoming applause, as they all turn and look, right at you.
"""
    , given (InteractionWith Moderator) (Always)
        `do` []
        `narrate` Simple """
She smiles and nods at you politely, but her eyes say *"If you stall one more minute I'm going to wring your neck!*"
"""
    , given (InteractionWith Podium) (Not (WithItem Mic))
        `do` [ AddInventory Mic ]
        `narrate` Simple """
You take a deep breath and make your way on stage.  The audience falls silent.  All eyes are on you.  Better say something quick.

"Ahem.  Ladies, gentlemen, distinguished guests..."

Um... what now?
"""
    , given (InteractionWith Hallway) (All [ NearProp Podium ])
        `do` []
        `narrate` Simple strangerPreventsYouFromLeaving
    , given (InteractionWith Kitchen) (All [ NearProp Podium ])
        `do` []
        `narrate` Simple strangerPreventsYouFromLeaving
    , given (InteractionWith Envelope) (WithItem Mic)
        `do` [ RemoveInventory Mic, LoadScene Middle ]
        `narrate` Simple """
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


middle : Scene MyStoryElement MyScene
middle =
    [ given (InteractionWith Envelope) (Always)
        `do` []
        `narrate` Simple "That went as well as could be expected.  Wonder where it came from?"
    ]


strangerPreventsYouFromLeaving : String
strangerPreventsYouFromLeaving =
    "You think about escaping, but both the moderator and the anxious Volunteer glare at you as if to say *\"Don't even think about it!\"*"


type MyScene
    = Beginning
    | Middle


type MyStoryElement
    = Envelope
    | Podium
    | Mic
    | Volunteer
    | Moderator
    | Kitchen
    | Auditorium
    | Hallway
    | KitchenExit
