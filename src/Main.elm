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
    , storyLine = [ """
You are in the commercial kitchen.  You don't know how you got here, or what you are doing here.

The only other person here is an anxious young man trying hard to get your attention.
    """ ]
    , itemsByLocation = Dict.singleton (toString Auditorium) [ Podium ] |> Dict.insert (toString Kitchen) [ KitchenExit ]
    , charactersByLocation = Dict.singleton (toString Kitchen) [ Stranger ] |> Dict.insert (toString Auditorium) [ Moderator ]
    }


storyElements : StoryElementsConfig MyStoryElement
storyElements element =
    case element of
        Stranger ->
            DisplayInformation "Anxious stranger" "He sure seems stressed.  You're not helping."

        Moderator ->
            DisplayInformation "Moderator" "She seems to command the audience and speakers with ease.  This definitely isn't her first rodeo."

        Kitchen ->
            DisplayInformation "Commercial kitchen" "Rows of stainless steel counters fill the floor, with banks of ovens, stoves, and walk-in fridges along the walls."

        Auditorium ->
            DisplayInformation "Auditorium" "The room is huge, every seat filled with the eager audience."

        Hallway ->
            DisplayInformation "Hallway" "Just outside the main auditorium, where a few straglers wander by with hot drinks or converse in hushed debates."

        KitchenExit ->
            DisplayInformation "Emergency exit" "The handle has a warning: \"Opening will sound the alarm\""

        Envelope ->
            DisplayInformation "Thick envelope" "You found it crammed in your pocket, but you don't recoginze it."

        Podium ->
            DisplayInformation "Podium" "The podium is on the stage, facing the audience.  Right in the spotlight."

        Mic ->
            DisplayInformation "Microphone" "\"Testing one, two.\" Yup, it works fine."


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
A way out.  You head for the emergency exit, but the stranger stops you.

"You can't leave!  Everyone is waiting for you!"
"""
    , given (InteractionWith Stranger) (InLocation Kitchen)
        `do` [ AddLocation Auditorium ]
        `narrate` Simple """
"Finally!  You drifted off for a minute there.  Come on, they are ready for you in auditorium.  Let's go."
"""
    , given (InteractionWith Auditorium) (InLocation Kitchen)
        `do` [ MoveTo Auditorium, AddCharacter Stranger Auditorium, AddLocation Hallway ]
        `narrate` Simple """
You follow the stranger into the auditorium.  Stepping in, you see the large room, packed with eager audience members.

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
    , given (InteractionWith Hallway) (All [NearProp Podium])
        `do` []
        `narrate` Simple strangerPreventsYouFromLeaving
    , given (InteractionWith Kitchen) (All [NearProp Podium])
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
    "You think about escaping, but both the moderator and the anxious stranger glare at you as if to say *\"Don't even think about it!\"*"


type MyScene
    = Beginning
    | Middle


type MyStoryElement
    = Envelope
    | Podium
    | Mic
    | Stranger
    | Moderator
    | Kitchen
    | Auditorium
    | Hallway
    | KitchenExit
