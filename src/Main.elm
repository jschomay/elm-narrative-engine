module Main exposing (..)

import Engine exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import Components.Storyline exposing (..)


-- main : Program Never


main =
    -- 3 things needed:
    -- - load all data - locations, items, characters, narratives
    -- - set initial data
    -- - provide update function(s)
    Engine.start
        ({ locations = initialLocations
         , inventory = initialItems
         , story = initialStory
         }
        )
        userTookAction


type PinkletonStoryLocations
    = Marsh
    | Greenhouse
    | Moon


initialLocations : Locations PinkletonStoryLocations
initialLocations =
    [ Location Marsh "Grassy Marsh" True
    , Location Greenhouse "Greenhouse" True
    , Location Moon "The Moon" False
    ]


type PinkletonStoryItems
    = Marble
    | Photograph
    | Umbrella


initialItems : Items PinkletonStoryItems
initialItems =
    [ Item Marble "Shiny marble"
    , Item Photograph "Torn Photograph"
    , Item Umbrella "Umbrella"
    ]


type PinkletonStoryProse
    = Beginning
    | UmbrellaDescription


initialStory =
    [ Story Beginning "It all started this morning over tea in the greenhouse.  I got a message from my friend Barrowmore to meet him in the marsh." ]


userTookAction action state =
    case action of
        Components.Inventory.InteractWithItem item ->
            reactToInteractionWithItem item state



-- GoToLocation
-- InteractWithCharacter
-- does there need to be a UseItem and TakeItem, or can it all be InteractWithItem?


reactToInteractionWithItem item state =
    case item of
        Umbrella ->
            -- I dont want to put the string here, I probably want to call an api (say UmbrellaDescription, sayFrom UmbrellaDescriptions, addLocation Marsh, etc)
            Story UmbrellaDescription "My trusty umbrella................"

        _ ->
            -- if state.currentSummary.characters `contains` Character Barrowmore then say Story KnockOnHead else sayFrom UmbrellaDescriptions Randomly
            Story UmbrellaDescription "Something else..."



{-
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
-}
