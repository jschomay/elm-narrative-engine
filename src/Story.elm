module Story
    exposing
        ( load
        , Info
        , Setup
        , storyWorld
        , StoryWorld
        , ItemInfo
        , LocationInfo
        , CharacterInfo
        , itemInfo
        , locationInfo
        , characterInfo
        , item
        , location
        , character
        , Rule
        , withItem
        , nearCharacter
        , nearItem
        , inLocation
        , withKnowledge
        , unless
        , moveTo
        , addLocation
        , removeLocation
        , addInventory
        , removeInventory
        , moveCharacter
        , removeCharacter
        , placeItem
        , removeItem
        , addKnowledge
        , loadScene
        , endStory
        )

{-| Entry point to the framework, where the client passes off all the story information.

The framework takes care of managing all of the state, views, and interaction handling, allowing the client code to focus purely on the story.

# Loading the story

@docs load, Info, Setup

# Defining your story world

@docs storyWorld, StoryWorld, ItemInfo, LocationInfo, CharacterInfo, itemInfo, locationInfo, characterInfo, item, location, character

# Defining story rules

Rules are declarative pairings of a "matcher" and a set of "commands" to perform if the rule matches.  Rules are grouped into "scenes" for better control and organization.  If no rules match, the framework will perform a default rule, which is usually just to narrate the description of what was interacted with.


A rule has four parts:

- a matcher against what displayable story element the user clicked on
- a list of conditions that all must match for the rule to match
- a list of changes to make if the rule matches
- what to add to the story line if the rule matches

    scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
    scene1 =
        [ { interaction = character Harry
          , conditions = [ inLocation Garden ]
          , changes = [ addCharacter Harry Marsh, removeCharacter Harry Garden ]
          , narration = ["Meet me in the marsh..."]
          }
        , { interaction = character Harry
          , conditions = [ inLocation Marsh ]
          , changes = []
          , narration = ["My good friend Harry...", "I wonder what he wants to tell me..."]
          }
        ]

When a rule matches multiple times (a player clicks the same story element multiple times), it will run through the list of narrations in order, one per click, repeating the final one when it reaches the end.

@docs Rule

## Conditions

The following condition matchers can be used in the `conditions` part of the rule record.

@docs  withItem , nearCharacter , nearItem , inLocation , withKnowledge , unless


## Changing the story world

You cannot change the story directly, but you can supply "commands" describing how the story state should change.

@docs moveTo, addLocation, removeLocation, addInventory, removeInventory, moveCharacter, removeCharacter, placeItem, removeItem, addKnowledge, loadScene, endStory

-}

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (..)
import Color exposing (Color)
import Types exposing (..)
import Types exposing (..)
import Story.Mechanics exposing (..)
import Story.State exposing (..)
import Views.Game exposing (..)


{-| A displayable story element -- and item, location, or character in your story that can be displayed and interacted with.
-}
type alias Displayable item location character =
    Types.Displayable item location character


{-| A means of looking up static information about your story displayables, which gets loaded into `Story.load`.
-}
storyWorld : (item -> ItemInfo) -> (location -> LocationInfo) -> (character -> CharacterInfo) -> StoryWorld item location character
storyWorld items locations characters =
    { items = items
    , locations = locations
    , characters = characters
    }


{-| A collection of all of the displayable elements in your story, for loading into the engine.
-}
type alias StoryWorld item location character =
    Types.StoryWorld item location character


{-| Display information for your items, including a name and description.  The description allows markdown.
-}
type alias ItemInfo =
    Types.ItemInfo


{-| Display information for your locations, including a name, a highlight color, and a description.  The description allows markdown.
-}
type alias LocationInfo =
    Types.LocationInfo


{-| Display information for your characters, including a name and description.  The description allows markdown.
-}
type alias CharacterInfo =
    Types.CharacterInfo


{-|
    itemInfo "Umbrella" "My trusty umbrella, I take it everywhere."
-}
itemInfo : String -> String -> ItemInfo
itemInfo name description =
    { name = name
    , description = description
    }


{-|
    locationInfo "Home" Color.Green "Home sweet home..."
-}
locationInfo : String -> Color -> String -> LocationInfo
locationInfo name color description =
    { name = name
    , description = description
    , color = color
    }


{-|
    characterInfo "Harry" "My good friend Harry..."
-}
characterInfo : String -> String -> CharacterInfo
characterInfo name description =
    { name = name
    , description = description
    }


{-| Wrap your item type in an `Displayable` type when matching interactions in your rules.
-}
item : item -> Displayable item location character
item =
    Item


{-| Wrap your location type in an `Displayable` type when matching interactions in your rules.
-}
location : location -> Displayable item location character
location =
    Location


{-| Wrap your character type in an `Displayable` type when matching interactions in your rules.
-}
character : character -> Displayable item location character
character =
    Character


type alias Model item location character knowledge =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , storyState : StoryState item location character knowledge
    }


{-| Information for the starting state of your story.  See the "Changing the story world" section for more information on the setupCommands.

    setup : Setup MyItem MyLocation MyCharacter MyKnowledge
    setup =
        { startingScene = beginning
        , startingLocation = Home
        , startingNarration = "Home sweet home..."
        , setupCommands =
            [ addLocation Conservatory
            , moveCharacter John Conservatory
            , placeItem Umbrella Home
            ]
        }
-}
type alias Setup item location character knowledge =
    { startingScene : List (Rule item location character knowledge)
    , startingLocation : location
    , startingNarration : String
    , setupCommands : List (ChangeWorldCommand item location character knowledge)
    }


{-| Basic information about the story, which gets displayed on the "title page" before beginning the story.

    info : Info
    info =
        { title = "The Continuing Adventures of Bartholomew Barrymore"
        , byline = "B. Barrymore"
        , prologue = "Mr. Barrymore is at it again, with more shenanigans in this comedy-mystery..."
        }
-}
type alias Info =
    { title : String
    , byline : String
    , prologue : String
    }


type Route
    = TitlePage
    | GamePage


type Msg item location character
    = NoOp
    | StartGame
    | GamePlay (Story.Mechanics.Msg item location character)


init : Info -> Setup item location character knowledge -> Model item location character knowledge
init { title, byline, prologue } setup =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , storyState = setUpWorld setup
    }


setUpWorld : Setup item location character knowledge -> StoryState item location character knowledge
setUpWorld { startingScene, startingLocation, startingNarration, setupCommands } =
    Story.State.init startingLocation startingScene
        |> \storyState -> Story.State.advanceStory "Begin" storyState setupCommands startingNarration


{-| This is where you load all of your story details into the framework (from the client's `Main.elm` file).  See https://github.com/jschomay/elm-interactive-story-starter.git for an example of how to define displayables and scenes.

    main : Program Never
    main =
        Story.load info displayables setup

-}
load :
    Info
    -> StoryWorld item location character
    -> Setup item location character knowledge
    -> Program Never
load info displayables setup =
    Html.beginnerProgram
        { model = init info setup
        , view = view displayables
        , update = update displayables
        }


{-| A declarative rule, describing how to advance your story and under what conditions.
-}
type alias Rule item location character knowledge =
    Types.Rule item location character knowledge


{-| Will match if the supplied item is in the inventory.
-}
withItem : item -> Condition item location character knowledge
withItem =
    WithItem


{-| Will match if the supplied character in in the current location.
-}
nearCharacter : character -> Condition item location character knowledge
nearCharacter =
    NearCharacter


{-| Will match if the supplied item is in the current location.  Ignores inventory.
-}
nearItem : item -> Condition item location character knowledge
nearItem =
    NearItem


{-| Will match when the supplied location is the current location.
-}
inLocation : location -> Condition item location character knowledge
inLocation =
    InLocation


{-| Will match if the specified knowledge has been acquired.
-}
withKnowledge : knowledge -> Condition item location character knowledge
withKnowledge =
    WithKnowledge


{-| Will match if the supplied condition does NOT match.
-}
unless : Condition item location character knowledge -> Condition item location character knowledge
unless =
    Unless


{-| Changes the current location.  The current location will be highlighted in the list of known locations, and will also be displayed at the top of the page, highlighted in the color defined for that location.  Any items or characters that are in the current location will also be shown for the player to interact with.

    moveTo Conservatory
-}
moveTo : location -> ChangeWorldCommand item location character knowledge
moveTo =
    MoveTo


{-| Adds a location to your list of known locations.  Any location on this list is available for the player to click on at any time.  This avoids clunky spatial navigation mechanics, but does mean that you will need to make rules to prevent against going to locations that are inaccessible (with appropriate narration).

    addLocation Conservatory
-}
addLocation : location -> ChangeWorldCommand item location character knowledge
addLocation =
    AddLocation


{-| Removes a location from your list of known locations.  You probably don't need this since once you know about a location you would always know about it, and trying to go to a location that is inaccessible for some reason could just give some narration telling why.  But maybe you will find a good reason to use it.

    removeLocation Home
-}
removeLocation : location -> ChangeWorldCommand item location character knowledge
removeLocation =
    RemoveLocation


{-| Adds an item to your inventory (if it was previously in a location, it will be removed from there, as items can only be in one place at once).

    addInventory Umbrella
-}
addInventory : item -> ChangeWorldCommand item location character knowledge
addInventory =
    AddInventory


{-| Removes an item from your inventory.  The item will not show up anywhere until you add it back to your inventory or to a location.

    removeInventory Umbrella
-}
removeInventory : item -> ChangeWorldCommand item location character knowledge
removeInventory =
    RemoveInventory


{-| Adds a character to a location, or moves a character to a different location (characters can only be in one location at a time, or off-screen).  (Use moveTo to move yourself between locations.)

    moveCharacter John Conservatory
-}
moveCharacter : character -> location -> ChangeWorldCommand item location character knowledge
moveCharacter =
    MoveCharacter


{-| Moves a character "off-screen".  The character will not show up in any locations until you use `moveCharacter` again.

    removeCharacter John
-}
removeCharacter : character -> ChangeWorldCommand item location character knowledge
removeCharacter =
    RemoveCharacter


{-| Move an item to a location.  If it was in another location or your inventory before, it will remove it from there, as items can only be in one place at once.

    placeItem Umbrella Home
-}
placeItem : item -> location -> ChangeWorldCommand item location character knowledge
placeItem =
    PlaceItem


{-| Moves an item "off-screen" (either from a location or the inventory).  The item will not show up in any locations or inventory until you use `placeItem` or `addInventory` again.

    removeItem Umbrella
-}
removeItem : item -> ChangeWorldCommand item location character knowledge
removeItem =
    RemoveItem


{-| Knowledge is an intangible "flag" that you can match against in your rules.  For example if you add knowledge of learning about a suspect, then going back to people you have already interacted with can give you new information about the suspect when you interact with them again.  You can also use this for acquiring skills or bonuses or anything intangible that would not be displayed in the story.  You could track your actions, such as if you were kind or mean to an important character in an earlier scene.

However, before turning to this tool, consider if you can use a normal, displayable story displayable instead.  For example, perhaps you get a sketch of the suspect in your inventory, which you can "show" to people for more information.  This keeps the story more concrete.

    type MyKnowledge = LearnOfSuspect | WrongSuspect | Amnesia

    addKnowledge LearnOfSuspect
-}
addKnowledge : knowledge -> ChangeWorldCommand item location character knowledge
addKnowledge =
    AddKnowledge


{-| Rules are grouped into "scenes" for better organization and control.  This is how you switch between scenes when you want a different rule set.  You may want to switch scenes at a "turning point" in your story to bring about new rules for the next objective.

    type MyScene = CallToAction | SearchForClues | CatchSuspect | etc...

    loadScene SearchForClues
-}
loadScene : List (Rule item location character knowledge) -> ChangeWorldCommand item location character knowledge
loadScene =
    LoadScene


{-| Let the framework know the story has ended.  Currently this has no effect, I'm trying to figure out what should happen when stories end.
-}
endStory : ChangeWorldCommand item location character knowledge
endStory =
    EndStory


update :
    StoryWorld item location character
    -> Msg item location character
    -> Model item location character knowledge
    -> Model item location character knowledge
update displayInfo msg model =
    case msg of
        NoOp ->
            model

        StartGame ->
            { model | route = GamePage }

        GamePlay msg ->
            { model | storyState = Story.Mechanics.update displayInfo msg model.storyState }



-- VIEW


view : StoryWorld item location character -> Model item location character knowledge -> Html (Msg item location character)
view displayInfo model =
    case model.route of
        TitlePage ->
            titelPage model

        GamePage ->
            Html.map GamePlay <| Views.Game.view displayInfo model.storyState


titelPage : Model item location character knowledge -> Html (Msg item location character)
titelPage model =
    div [ class "TitlePage" ]
        [ h1 [ class "TitlePage__Title" ] [ text model.title ]
        , h3 [ class "TitlePage__Byline" ] [ text <| "An interactive story by " ++ model.byline ]
        , toHtml [ class "TitlePage__Prologue markdown-body" ] model.prologue
        , span [ class "TitlePage__StartGame", onClick StartGame ] [ text "Play" ]
        ]
