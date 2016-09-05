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
            location "commercial kitchen" blue "Everything has been cleaned, polished, and put away for the next morning."

        Auditorium ->
            location "auditorium 'A'" purple "The lights are low, the plush velvet seats filled to capacity.  A spotlight shines down on the podium on the stage."

        Hallway ->
            location "hallway" darkOrange "Just outside the main auditorium, where a few stragglers wander around with hot drinks, or converse in hushed debates about the previous presentations."


