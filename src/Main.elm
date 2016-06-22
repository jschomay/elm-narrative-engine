module Main exposing (..)

import Engine exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import Components.Storyline exposing (..)


main : Program Never
main =
    Engine.load
        { locations = initialLocations
        , inventory = initialItems
        , story = initialStory
        }


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


initialStory =
    [ Story Beginning "It all started this morning over tea in the greenhouse.  I got a message from my friend Barrowmore to meet him in the marsh." ]
