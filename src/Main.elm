module Main exposing (..)

import Engine exposing (..)
import Engine exposing (..)
import StoryWorld exposing (..)
import Scenes exposing (..)


main : Program Never
main =
    loadStory "Stage Fright" storyElements initialSetup


initialSetup : InitialSetup MyStoryElements
initialSetup =
    { scene = introScene
    , location = Kitchen
    , inventory = [ Watch ]
    , knownLocations = [ Kitchen, BackDoor, Auditorium ]
    , characters = [ Stranger ]
    , intro = "Well, here I am..."
    , props = [ Envelope ]
    }


storyElements : List (StoryElement MyStoryElements)
storyElements =
    [ StoryElement Envelope "Unfamilar Envelope" "You find an unfamilar envelope in your pocket, stuffed with thickly folded papers."
    , StoryElement Kitchen "Commercial kitchen" "Clean and steril, with rows of oversized ovens, stainless steel counters, and lots of pots and pans.  No chefs or cooks of any kind though."
    , StoryElement Stranger "A stranger" "I've never seen him before, but he seems very adamant about getting my attention."
    , StoryElement Watch "Wristwatch" "That's strange, it doesn't have any numbers on it..."
    , StoryElement BackDoor "Back door" "It looks like an exit out of here."
    , StoryElement Auditorium "Auditorium" "Oh crap this room is huge.  There must be over a hundred people in the audience... all looking at me expectedly.  Yikes."
    , StoryElement Podium "Podium" "The scariest seat in the house.  Looks like it's reserved for me."
    , StoryElement NervousPresenter "Nervous man" "Pacing back and forth, he looks even more nervous than me.  He keeps muttering to himself."
    ]


introScene : Scene MyStoryElements
introScene =
    [ StoryRule (Given (InteractionWith Envelope) (WithOut [Envelope]))
        (Do [ AddInventory Envelope ] (Narrate (Simple "A mysterious envelope, I'll take that.")))
    , StoryRule (Given (InteractionWith Envelope) (In [ Auditorium ]))
        (Do [] (Narrate (Simple "Ladies and gentlemen.... my speech...")))
    , StoryRule (Given (InteractionWith Envelope) (Near [ NervousPresenter ]))
        (Do [] (Narrate (Simple "Is this yours?  Yes!! Thanks!!")))
    , StoryRule (Given (InteractionWith Auditorium) (Always))
        (Do [ MoveTo Auditorium, AddProp Podium ] (Narrate (Simple "I hesitantly went into the auditorium...")))
    ]


type MyStoryElements
    = Envelope
    | Watch
    | Kitchen
    | Stranger
    | Podium
    | BackDoor
    | NervousPresenter
    | Auditorium
