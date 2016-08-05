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
    , inventory = [Envelope, Watch]
    , knownLocations = [Kitchen, BackDoor, Auditorium]
    , characters = [Stranger]
    , intro = "Well, here I am..."
    , props = [Envelope, Kitchen, Kitchen]
    }

storyElements : List (StoryElement MyStoryElements)
storyElements =
    [ StoryElement Envelope "Unfamilar Envelope" "You find an unfamilar envelope in your pocket, stuffed with thickly folded papers."
    , StoryElement Kitchen "Commercial kitchen" "Clean and steril, with rows of oversized ovens, stainless steel counters, and lots of pots and pans.  No chefs or cooks of any kind though."
    , StoryElement Stranger "A stranger" "I've never seen him before, but he seems very adamant about getting my attention."
    , StoryElement Watch "Wristwatch" "That's strange, it doesn't have any numbers on it..."
    , StoryElement BackDoor "Back door" "It looks like an exit out of here."
    , StoryElement Auditorium "Auditorium" "Oh crap this room is huge.  There must be over a hundred people in the audience... all looking at me expectedly.  Yikes."
    ]

introScene : Scene MyStoryElements
introScene =
    []

type MyStoryElements
    = Envelope
    | Watch
    | Kitchen
    | Stranger
    | BackDoor
    | Auditorium

-- type MyItems
--     = Umbrella


-- type MyCharacters
--     = Bosco



-- storyWorld : StoryWorld a
-- storyWorld =
--     [ item Umbrella "My Brolly" "I take it everywhere"
--     , character Bosco "Mr. Bosco" "What a jerk"
--     ]
--





--
{-

   StoryRule (InteractionWith Envelope) (With Podium) [] ReadSpeach
   StoryRule
     Given
       ((InteractionWith MysteryMan) (WithOut Auditorium))
     Then
       [AddLocation Auditorium]
       Narrate InOrder
         [ "They're all waiting for you, go out there!"
         , "Get moving!"
         ]
   -
-}
