module Locations exposing (..)

import Color exposing (..)
import Story.Element exposing (..)


type MyLocation
    = Kitchen
    | Auditorium
    | Hallway


locations : MyLocation -> LocationInfo
locations location =
    case location of
        Kitchen ->
            locationInfo "commercial kitchen" blue "Everything has been cleaned, polished, and put away for the next morning."

        Auditorium ->
            locationInfo "auditorium 'A'" purple "The lights are low, the plush velvet seats filled to capacity.  A spotlight shines down on the podium on the stage."

        Hallway ->
            locationInfo "hallway" darkOrange "Just outside the main auditorium, where a few stragglers wander around with hot drinks, or converse in hushed debates about the previous presentations."
