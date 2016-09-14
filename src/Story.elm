module Story
    exposing
        ( load
        , Info
        , Setup
        , Elements
        , storyWorld
        , ItemInfo
        , LocationInfo
        , CharacterInfo
        , itemInfo
        , locationInfo
        , characterInfo
        , item
        , location
        , character
        , Scene
        , firstInteractionWith
        , interactingWith
        , when
        , changesWorld
        , narrates
        , everyTime
        , withItem
        , nearCharacter
        , nearProp
        , inLocation
        , withKnowledge
        , all
        , any
        , unless
        , moveTo
        , addLocation
        , removeLocation
        , addInventory
        , removeInventory
        , addCharacter
        , removeCharacter
        , addProp
        , removeProp
        , addKnowledge
        , loadScene
        , endStory
        )

{-| Entry point to the framework, where the client passes off all the story information.

The framework takes care of managing all of the state, views, and interaction handling, allowing the client code to focus purely on the story.

# Loading the story

@docs load, Info, Setup

# Defining your story world

@docs Elements, storyWorld, ItemInfo, LocationInfo, CharacterInfo, itemInfo, locationInfo, characterInfo, item, location, character

# Defining story rules

Rules are declarative pairings of a "matcher" and a set of "commands" to perform if the rule matches.  Rules are grouped into "scenes" for better control and organization.  If no rules match, the framework will perform a default rule, which is usually just to narrate the description of what was interacted with.

@docs Scene

You build up rules via a declarative DSL like this:

    storyRules =
        [ interactingWith (item Umbrella)
            `when` (unless (withItem Umbrella))
            `changesWorld` [ addInventory Umbrella ]
            `narrates` "I always take my umbrella with me."
        , interactingWith (item Umbrella)
            `when` (inLocation Swamp)
            `changesWorld` [ ]
            `narrates` "Good thing I brought my umbrella!"
        ...
        ]

@docs firstInteractionWith, interactingWith, when, changesWorld, narrates


## Matching rules

The following condition matchers can be used in the `when` part of the DSL.  Note that you can nest and combine these matchers with `unless`, `all`, and `any`.

@docs everyTime , withItem , nearCharacter , nearProp , inLocation , withKnowledge , all , any , unless


## Changing the story world

You cannot change the story directly, but you can supply "commands" describing how the story state should change, as well as "narration" to accompany these changes.


@docs moveTo, addLocation, removeLocation, addInventory, removeInventory, addCharacter, removeCharacter, addProp, removeProp, addKnowledge, loadScene, endStory
-}

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (..)
import Color exposing (Color)
import Story.Element exposing (..)
import Story.Rule exposing (..)
import Story.State exposing (..)
import Story.Mechanics exposing (..)
import Views.Game exposing (..)


{-| The items, locations, and characters in your story that will be displayed and interacted with.
-}
type alias Elements a b c =
    Story.Element.Elements a b c


{-| A means of looking up static information about your story elements, which gets loaded into `Story.load`.
-}
storyWorld : (a -> ItemInfo) -> (b -> LocationInfo) -> (c -> CharacterInfo) -> Elements a b c
storyWorld items locations characters =
    { items = items
    , locations = locations
    , characters = characters
    }


{-| Display information for your items, including a name and description.  The description allows markdown.
-}
type alias ItemInfo =
    Story.Element.ItemInfo


{-| Display information for your locations, including a name, a highlight color, and a description.  The description allows markdown.
-}
type alias LocationInfo =
    Story.Element.LocationInfo


{-| Display information for your characters, including a name and description.  The description allows markdown.
-}
type alias CharacterInfo =
    Story.Element.CharacterInfo


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


{-| Wrap your item type in an `Element` type
-}
item : a -> Element a b c
item =
    Story.Element.Item


{-| Wrap your location type in an `Element` type
-}
location : b -> Element a b c
location =
    Story.Element.Location


{-| Wrap your character type in an `Element` type
-}
character : c -> Element a b c
character =
    Story.Element.Character


type alias Model a b c d e =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , storyState : StoryState a b c d e
    }


{-| Information for the starting state of your story.  See the "Changing the story world" section for more information on the setupCommands.

    setup : Setup MyItem MyLocation MyCharacter MyScene MyKnowledge
    setup =
        { startingScene = Beginning
        , startingLocation = Home
        , startingNarration = "Home sweet home..."
        , setupCommands =
            [ addLocation Conservatory
            , addCharacter John Conservatory
            , addProp Umbrella Home
            ]
        }
-}
type alias Setup a b c d e =
    { startingScene : d
    , startingLocation : b
    , startingNarration : String
    , setupCommands : ChangeWorldCommands a b c d e
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


type Msg a b c
    = NoOp
    | StartGame
    | Interaction (Story.Mechanics.Msg a b c)


init : Info -> Setup a b c d e -> Model a b c d e
init { title, byline, prologue } setup =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , storyState = setUpWorld setup
    }


setUpWorld : Setup a b c d e -> StoryState a b c d e
setUpWorld { startingScene, startingLocation, startingNarration, setupCommands } =
    Story.State.init startingLocation startingScene
        |> \storyState -> Story.State.advanceStory "Begin" storyState ( setupCommands, Narrate startingNarration )


{-| This is where you load all of your story details into the framework (from the client's `Main.elm` file).  See https://github.com/jschomay/elm-interactive-story-starter.git for an example of how to define elements and scenes.

    main : Program Never
    main =
        Story.load info elements scenes setup

-}
load : Info -> Elements a b c -> (d -> Scene a b c d e) -> Setup a b c d e -> Program Never
load info elements scenes setup =
    Html.beginnerProgram
        { model = init info setup
        , view = view elements
        , update = update elements scenes
        }



-- UPDATE


{-| A list of declarative rules, describing how to advance your story.
-}
type alias Scene a b c d e =
    Story.Rule.Scene a b c d e


{-| Part of the rule building DSL. See the example above.   `firstInteractionWith` is interchangeable with `interactingWith`, and will only match on the first interaction with the give story element.
-}
firstInteractionWith : Element a b c -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
firstInteractionWith element condition advanceStory =
    ( ( FirstInteractionWith element, condition ), advanceStory )


{-| Part of the rule building DSL. See the example above.
-}
interactingWith : Element a b c -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
interactingWith element condition advanceStory =
    ( ( InteractionWith element, condition ), advanceStory )


{-| Part of the rule building DSL. See the example above.
-}
when : (Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e) -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
when f condition =
    f condition


{-| Part of the rule building DSL. See the example above.
-}
changesWorld : (AdvanceStory a b c d e -> Rule a b c d e) -> ChangeWorldCommands a b c d e -> Narration -> Rule a b c d e
changesWorld f a b =
    f ( a, b )


{-| Part of the rule building DSL. See the example above.
-}
narrates : (Narration -> Rule a b c d e) -> String -> Rule a b c d e
narrates f =
    f << Narrate


{-| Will match every time.  Useful in combination with `FirstInteractionWith` (though you may want to restrict that more).  Also useful when not supplying any commands to change the world, but you do want a custom narration.
-}
everyTime : Condition a b c e
everyTime =
    EveryTime


{-| Will match if the supplied item is in the inventory.
-}
withItem : a -> Condition a b c e
withItem =
    WithItem


{-| Will match if the supplied character in in the current location.
-}
nearCharacter : c -> Condition a b c e
nearCharacter =
    NearCharacter


{-| Will match if the supplied item in in the current location.
-}
nearProp : a -> Condition a b c e
nearProp =
    NearProp


{-| Will match when the supplied location is the current location.
-}
inLocation : b -> Condition a b c e
inLocation =
    InLocation


{-| Will match if the specified knowledge has been acquired.
-}
withKnowledge : e -> Condition a b c e
withKnowledge =
    WithKnowledge


{-| Will match if all of the supplied conditions match.
-}
all : List (Condition a b c e) -> Condition a b c e
all =
    All


{-| Will match if any of the supplied conditions match.
-}
any : List (Condition a b c e) -> Condition a b c e
any =
    Any


{-| Will match if the supplied condition does NOT match.
-}
unless : Condition a b c e -> Condition a b c e
unless =
    Unless


{-| Changes the current location.  The current location will be highlighted in the list of known locations, and will also be displayed at the top of the page, highlighted in the color defined for that location.  Any items or characters that are in the current location will also be shown for the player to interact with.

    moveTo Conservatory
-}
moveTo : b -> Story.State.ChangeWorldCommand a b c d e
moveTo =
    Story.State.MoveTo


{-| Adds a location to your list of known locations.  Any location on this list is available for the player to click on at any time.  This avoids clunky spatial navigation mechanics, but does mean that you will need to make rules to prevent against going to locations that are inaccessible (with appropriate narration).

    addLocation Conservatory
-}
addLocation : b -> Story.State.ChangeWorldCommand a b c d e
addLocation =
    Story.State.AddLocation


{-| Removes a location from your list of known locations.  You probably don't need this since once you know about a location you would always know about it, and trying to go to a location that is inaccessible for some reason could just give some narration telling why.  But maybe you will find a good reason to use it.

    removeLocation Home
-}
removeLocation : b -> Story.State.ChangeWorldCommand a b c d e
removeLocation =
    Story.State.RemoveLocation


{-| Adds an item to your inventory.

    addInventory Umbrella
-}
addInventory : a -> Story.State.ChangeWorldCommand a b c d e
addInventory =
    Story.State.AddInventory


{-| Removes an item from your inventory.  The item will not show up anywhere until you add it back to your inventory or to a location.

    removeInventory Umbrella
-}
removeInventory : a -> Story.State.ChangeWorldCommand a b c d e
removeInventory =
    Story.State.RemoveInventory


{-| Adds a character to a location.  (Use moveTo to move yourself between locations.)

    addCharacter John Conservatory
-}
addCharacter : c -> b -> Story.State.ChangeWorldCommand a b c d e
addCharacter =
    Story.State.AddCharacter


{-| Removes a character from a location.  The character will not show up anywhere else until you add it to another location.  (Use moveTo to move yourself between locations.)

    removeCharacter John Conservatory
-}
removeCharacter : c -> b -> Story.State.ChangeWorldCommand a b c d e
removeCharacter =
    Story.State.RemoveCharacter


{-| "Props" are just items in a location instead of in your inventory.  They could be something fixed to the location, such as a painting on a wall that you can look at but not take, or they can be something that you can take into your inventory.

This command adds an item to a location.

    addProp Umbrella Home
-}
addProp : a -> b -> Story.State.ChangeWorldCommand a b c d e
addProp =
    Story.State.AddProp


{-| Removes an item from a location.

    removeProp Umbrella Home
-}
removeProp : a -> b -> Story.State.ChangeWorldCommand a b c d e
removeProp =
    Story.State.RemoveProp


{-| Knowledge is an intangible "flag" that you can match against in your rules.  For example if you add knowledge of learning about a suspect, then going back to people you have already interacted with can give you new information about the suspect when you interact with them again.  You can also use this for acquiring skills or bonuses or anything intangible that would not be displayed in the story.  You could track your actions, such as if you were kind or mean to an important character in an earlier scene.

However, before turning to this tool, consider if you can use a normal, displayable story element instead.  For example, perhaps you get a sketch of the suspect in your inventory, which you can "show" to people for more information.  This keeps the story more concrete.

    type MyKnowledge = LearnOfSuspect | WrongSuspect | Amnesia

    addKnowledge LearnOfSuspect
-}
addKnowledge : e -> Story.State.ChangeWorldCommand a b c d e
addKnowledge =
    Story.State.AddKnowledge


{-| Rules are grouped into "scenes" for better organization and control.  This is how you switch between scenes when you want a different rule set.  You may want to switch scenes at a "turning point" in your story to bring about new rules for the next objective.

    type MyScene = CallToAction | SearchForClues | CatchSuspect | etc...

    loadScene SearchForClues
-}
loadScene : d -> Story.State.ChangeWorldCommand a b c d e
loadScene =
    Story.State.LoadScene


{-| Let the framework know the story has ended.  Currently this has no effect, I'm trying to figure out what should happen when stories end.
-}
endStory : Story.State.ChangeWorldCommand a b c d e
endStory =
    Story.State.EndStory


update : Elements a b c -> (d -> Scene a b c d e) -> Msg a b c -> Model a b c d e -> Model a b c d e
update displayInfo scenes msg model =
    case msg of
        NoOp ->
            model

        StartGame ->
            { model | route = GamePage }

        Interaction msg ->
            { model | storyState = Story.Mechanics.update displayInfo scenes msg model.storyState }



-- VIEW


view : Elements a b c -> Model a b c d e -> Html (Msg a b c)
view displayInfo model =
    case model.route of
        TitlePage ->
            titelPage model

        GamePage ->
            Html.map Interaction <| Views.Game.view displayInfo model.storyState


titelPage : Model a b c d e -> Html (Msg a b c)
titelPage model =
    div [ class "TitlePage" ]
        [ h1 [ class "TitlePage__Title" ] [ text model.title ]
        , h3 [ class "TitlePage__Byline" ] [ text <| "An interactive story by " ++ model.byline ]
        , toHtml [ class "TitlePage__Prologue markdown-body" ] model.prologue
        , span [ class "TitlePage__StartGame", onClick StartGame ] [ text "Play" ]
        ]
