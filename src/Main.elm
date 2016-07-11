module Main exposing (..)

import Engine exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import Components.Storyline exposing (..)


-- main : Program Never


main =
    Engine.start narrativeContent
        advanceStoryLogic
        -- all of these should be commands since the client doesn't control the model and shouldn't know the nodes.....
        -- but how to do the initial send?
        { knownLocations = initialLocations
        , inventory = initialItems
        , story = initialStory
        }



-- type PinkletonStoryLocations
--     = Marsh
--     | Greenhouse
--     | Moon
-- initialLocations : KnownLocations PinkletonStoryLocations
-- initialLocations =
--     [ KnownLocation Marsh "Grassy Marsh" True
--     , KnownLocation Greenhouse "Greenhouse" True
--     , KnownLocation Moon "The Moon" False
--     ]
-- type PinkletonStoryItems
--     = Marble
--     | Photograph
--     | Umbrella
-- initialItems : Items PinkletonStoryItems
-- initialItems =
--     [ Item Marble "Shiny marble"
--     , Item Photograph "Torn Photograph"
--     , Item Umbrella "Umbrella"
--     ]
-- initialStory =
--     -- this won't include the text after changing over to initializing via commands
--     [ StoryText "It all started this morning over tea in the greenhouse.  I got a message from my friend Barrowmore to meet him in the marsh." ]
{-
   Lots of new ideas around defining all of conditions for each command and not needing a logic funciton

   Think of it as defining states and transitions


   The defaults are defined with the initial type
-}
-- just like function entry points and guards
-- scene1Rules =
--     [ InteractWith Umbrella Scenario (Exactely Umbrella) [ Narrate "my umbrella", RemoveItem Umbrella ]
--     , InteractWith Umbrella Scenario (All [ Umbrella Chainsaw ]) "sounds dangerous"
--     , InteractWith Chainsaw Scenario (Any) "oooh, fun"
--     ]


type StoryScene
    = Intro


type StoryElement
    = Greenhouse
    | Marsh
    | Umbrella
    | Marble
    | Barrowmore
    | Photograph


setup : List StoryCommand
setup =
    [ AddLocation Greenhouse []
    , AddLocation Marsh
    , AddToLocation Marsh [ Item Marble, Character Barrowmore ]
    , AddInventory Umbrella
    , AddInventory Photograph
    , Go Greenhouse
    , CueScene Intro
    ]


intro : Scene
intro =
    [ StoryRule Marsh Always [ Narrate "I set out for the marsh by foot.  After about an hour I arrived...." ]
    , StoryRule Umbrella (With Barrowmore) [ Narrate SmakingBarrowmore] -- this is more like it
    -- , StoryRule Umbrella (With Barrowmore) [ Narrate "He deserves a good bonk on the head, but that wouldn't be nice." ]
    , StoryRule Photograph
        (With Barrowmore)
        [ Narrate "I gave him the photograph..."
        , RemoveInventory Photograph
        , RemoveFromLocation Barrowmore
        ]
    , StoryRule AnyItem (With Barrowmore)
      [ NarrateFrom InOrder
        [ "Barrowmore says quit stalling"
        , "I dont have all day!"
        , "give me the photograph"
        ]
      ]
    ]



-- type Matcher a
--     = Any
--     | Exactly a
--     | OneOf (List a)
--     | All (List a)
-- type alias Scenario = {
--   condition : FrameworkMatcher
--   commands : List Command
--   }
-- type FrameworkMatcher a =
--   InteractWith a
--   | Scenario (Matcher a)
--   | And (FrameworkMatcher a) (FrameworkMatcher b)
-- narrativeContent =
--     [ StoryElement Umbrella "My brolly..." "this is the umbrella...."
--     , StoryElement UmbrellaInMarsh "My brolly..." "tut tut, looks like rain"
--     , StoryElement (Exactly Marsh) (Exactly Umbrella) [DisplayDescription "looks like rain"]
--     , StoryElement MarbleClicked Marble "a marble..." "nice, a shiny marble..."
--       -- , BarrowmoreDescriptions [ "1...", "2...", "3..." ]
--     ]
-- advanceStoryLogic : subject -> commands


advanceStoryLogic currentScene subjectInteractedWith surroundings =
  case currentScene of
    Intro ->
        introRules subjectInteractedWith surroundings

introRules subjectInteractedWith surroundings =
      case subjectInteractedWith of

         Umbrella ->
            if present surroundings [Barrowmore] then
                [ContinueStory smackingBarrowmore
                , RemoveInventory Umbrella
                ]
              else ContinueStoryFrom umbrellaDescriptions

         Marble ->
              ContinueStory Marble

         _ ->
              None
    _ ->
      None


-- option 1
-- as functions (top level in an imported module, used by the client only)
umbrellaDescriptions = StoryTextVarried UmbrellaDescription Randomly ["Just a brolly", "My trusty brolly", "Nice umbrella"]

smackingBarrowmore = "he deserves a good whack..."

type alias StoryText = String

type StoryTextVarried tag
  = StoryTextVarried tag CycleType (List StoryText)

-- need the tag as a key to store last used index
--
-- option 2
-- as datastructure processed by the framework
[ Narration UmbrellaDescriptions (Randomly ["Just a brolly", "My trusty brolly", "Nice umbrella"])
, Narration SmakingBarrowmore (Just "he deserves a good whack...")
]

type Narration tag
  = Narration tag CycleType

type CycleType =
  Just String
  | Randomly (List String)
  | InOrder (List String)

{-

MY DESCISION:
I want to use the data style StoryRules rather than the nested case of statements.
I like this because it feels good like writing the html structures, and I like
defining data rather than behavior in the client.

For the Narrate command I want to use a Narrattion tag instead of a string.  The main
reason for this is because I need to define cycling types once (on loading narrative content)
or things will get messed up if I use Randomly in on place and InOrder in another in different
rules for the same text (though that may not happen in practice?).
Also it separates the text from the data structure, which is nice. 
Also the story rules should not also be in charge of defining narration.
Unfortunately I have to make the tags either way to track cycling.

   notes...
   -----
   Maybe Loations, Items, Character and StoryText can all be one type?


   Problem - need a way to store different texts from the same thing, example, the umbrella desc vs hitting Barrowmore with it.
   Where to store it?
   Just passing the string in the scene update function would be simplest, but I want StoryContent and StoryLogic as 2 separate pieces.


   ---


      Architecture and API design notes

      Engine's update function checks action type and calls client's appropriate update function, which returns batched commands for the engine to update the model against

      Only 3 actions the engine responds to:
      - Move location
      - InteractWithItem item (take, use, give, inspect, drop)
      - InteractWithCharacter charater (talk to, inspect, attack, approach, etc..)
      (maybe just need one action -- AdvanceStory, which takes a location, item or character and responds accordingly)
      (later there may be others: Save, Load, PublishStory, Undo/ResetTo, etc)

      Commands a client can give to the engine:
      - reveal narrative (probably always happens - description, setting, dialog, remark, plot, etc...)
      - add/remove from inventory
      - add to known locations
      - make known location unavailable/available
      - add/remove item(s)/character(s) to current state
      - change current state
      - next scene
      - story completed (with optional ending type)

      Add a Scene type as the main state machine.  This could be the top level update function with its own special handling of important objects in that scene.  If it doesn't have a special handling, it defers to the item/character/location level update functions.  This would mean we also need a way of storing and advancing the scene (not necessarily the same thing as the current location)  Each scene could have an optional description/title, like a chapter heading to print when entering a new scene.

      other potential api helpers:
       - hasHappened : (Narrative a) -> Bool -- (implementation would also need to check model.storyline)
       - similarly, maybe hasMet and hasBeenTo and knowsAbout
       - hasMoreToSay
-}
