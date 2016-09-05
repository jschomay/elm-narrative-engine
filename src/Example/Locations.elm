module Locations exposing (..)

import Color exposing (..)

import StoryElements exposing (..)

type MyLocation
    = Kitchen
    | Auditorium
    | Hallway


storyLocations : LocationsInfo MyLocation
storyLocations tag =
    case tag of
        Kitchen ->
            location "commercial kitchen" blue "Rows of stainless steel counters fill the floor, with banks of ovens, stoves, and walk-in fridges along the walls."

        Auditorium ->
            location "auditorium" purple "The lights are low, the plush velvet seats filled to capacity.  A spotlight shines down on the podium on the stage."

        Hallway ->
            location "hallway" darkOrange "Just outside the main auditorium, where a few straglers wander by with hot drinks or converse in hushed debates."


