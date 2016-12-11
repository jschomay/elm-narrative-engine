module Engine
    exposing
        ( Model
        , Msg
        , itemMsg
        , locationMsg
        , characterMsg
        , rollbackMsg
        , init
        , update
        , getCurrentLocation
        , getNearByProps
        , getNearByCharacters
        , getInventory
        , getLocations
        , getStoryLine
        , StartingState
        , world
        , World
        , ItemInfo
        , LocationInfo
        , CharacterInfo
        , itemInfo
        , locationInfo
        , characterInfo
        , Rule
        , InteractionMatcher
        , withItem
        , withLocation
        , withCharacter
        , withAnything
        , withAnyItem
        , withAnyLocation
        , withAnyCharacter
        , Condition
        , withInventory
        , nearCharacter
        , nearItem
        , inLocation
        , withKnowledge
        , unless
        , ChangeWorldCommand
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

{-| The story engine handles storing and advancing your story state by running through your story rules on each interaction.  It allows the client code to handle building the story world, story rules, and display layer.

# Embedding the story engine

The story engine is designed to be embedded in your own Elm app, allowing for maximum flexibility and customization.

You can base your app on the [interactive story starter repo](https://github.com/jschomay/elm-interactive-story-starter.git).

@docs Model, Msg, itemMsg, locationMsg, characterMsg, rollbackMsg, init, update, getCurrentLocation, getNearByProps, getNearByCharacters, getInventory, getLocations, getStoryLine, StartingState

# Defining your story world

@docs world, World, ItemInfo, LocationInfo, CharacterInfo, itemInfo, locationInfo, characterInfo

# Defining story rules

Rules are how you progress the story.  They are made up of conditions to match against and commands to perform if the rule matches.  Rules are grouped into "scenes" for better control and organization.  The engine will run through the active scene from the beginning, looking for the first matching rule, then run it.  If no rules match, the framework will perform a default command, which is usually just to narrate the description of what was interacted with.


A rule has four parts:

1. A matcher against what interactable story element the user clicked on
2. A list of conditions that all must match for the rule to match
3. A list of changes to make if the rule matches
4. Narration to add to the story line if the rule matches (note that you can use markdown)


    scene1 : List (Engine.Rule MyItem MyLocation MyCharacter MyKnowledge)
    scene1 =
        [ { interaction = withCharacter Harry
          , conditions = [ inLocation Garden ]
          , changes = [ moveCharacter Harry Marsh, addInventory NoteFromHarry ]
          , narration = [ "He gives you a note, then runs off.", "I wonder what he wants?" ]
          }
        , { interaction = withInventory NoteFromHarry
          , conditions = []
          , changes = [ addLocation Marsh ]
          , narration = [ "It says, \"*Meet me in the marsh.*\"" ]
          }
        ]

When a rule matches multiple times (a player clicks the same story element multiple times), it will run through the list of narrations in order, one per click, repeating the final one when it reaches the end.

@docs Rule

## Interaction matchers

The following interaction matchers can be used in the `interaction` part of the rule record.

@docs InteractionMatcher, withItem, withLocation, withCharacter, withAnything, withAnyItem, withAnyLocation, withAnyCharacter


## Conditions

The following condition matchers can be used in the `conditions` part of the rule record.

@docs  Condition, withInventory , nearCharacter , nearItem , inLocation , withKnowledge , unless


## Changing the story world

You cannot change the story directly, but you can supply "commands" describing how the story state should change.

@docs ChangeWorldCommand, moveTo, addLocation, removeLocation, addInventory, removeInventory, moveCharacter, removeCharacter, placeItem, removeItem, addKnowledge, loadScene, endStory

-}

import Color exposing (Color)
import Types exposing (..)
import Types exposing (..)
import Engine.Mechanics exposing (..)
import Engine.State exposing (..)


{-| A interactable story element -- and item, location, or character in your story that can be displayed and interacted with.
-}
type alias Interactable item location character =
    Types.Interactable item location character


{-| A means of looking up static information about your story interactables, which gets loaded into `Engine.load`.
-}
world : (item -> ItemInfo) -> (location -> LocationInfo) -> (character -> CharacterInfo) -> World item location character
world items locations characters =
    { items = items
    , locations = locations
    , characters = characters
    }


{-| A collection of all of the interactable elements in your story, for loading into the engine.
-}
type alias World item location character =
    Types.World item location character


{-| -}
type alias ItemInfo =
    Types.ItemInfo


{-| -}
type alias LocationInfo =
    Types.LocationInfo


{-| -}
type alias CharacterInfo =
    Types.CharacterInfo


{-| Display information for your items, including a name and description.  The description allows markdown.

    itemInfo "Umbrella" "My trusty umbrella, I take it everywhere."
-}
itemInfo : String -> String -> ItemInfo
itemInfo name description =
    { name = name
    , description = description
    }


{-| Display information for your locations, including a name, a highlight color, and a description.  The description allows markdown.

    locationInfo "Home" Color.Green "Home sweet home..."
-}
locationInfo : String -> Color -> String -> LocationInfo
locationInfo name color description =
    { name = name
    , description = description
    , color = color
    }


{-| Display information for your characters, including a name and description.  The description allows markdown.

    characterInfo "Harry" "My good friend Harry..."
-}
characterInfo : String -> String -> CharacterInfo
characterInfo name description =
    { name = name
    , description = description
    }


{-| You'll need this type if you embed the engine in your own app.
-}
type Model item location character knowledge
    = Model (StoryHistory item location character knowledge)


{-| This too
-}
type alias Msg item location character =
    Types.Msg item location character


{-| Let the engine know the user interacted with an item
-}
itemMsg : item -> Types.Msg item location character
itemMsg =
    Interact << Item


{-| Let the engine know the user interacted with a location
-}
locationMsg : location -> Types.Msg item location character
locationMsg =
    Interact << Location


{-| Let the engine know the user interacted with a character
-}
characterMsg : character -> Types.Msg item location character
characterMsg =
    Interact << Character


{-| Let the engine know to "roll back" the story.  The `Int` indicates the number of interactions to keep.  For example, `Engine.rollbackMsg <| (List.length Engine.getStoryLine) - 1` would "undo" the last move, and `Engine.rollback 0` would "reset" the entire story.
-}
rollbackMsg : Int -> Types.Msg item location character
rollbackMsg =
    Rollback


{-| Information for the starting state of your story.  See the "Changing the story world" section for more information on the setupCommands.

    setup : StartingState MyItem MyLocation MyCharacter MyKnowledge
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
type alias StartingState item location character knowledge =
    { startingScene : List (Rule item location character knowledge)
    , startingLocation : location
    , startingNarration : String
    , setupCommands : List (ChangeWorldCommand item location character knowledge)
    }


setUpWorld : StartingState item location character knowledge -> StoryState item location character knowledge
setUpWorld { startingScene, startingLocation, startingNarration, setupCommands } =
    Engine.State.init startingLocation startingScene
        |> \storyState -> Engine.State.advanceStory "Begin" storyState setupCommands startingNarration


{-| Initialize the `Model` for use when embedding in your own app.
-}
init :
    StartingState item location character knowledge
    -> Model item location character knowledge
init setup =
    Model <| StoryHistory [] (setUpWorld setup)


{-| Get the current location to display
-}
getCurrentLocation :
    World item location character
    -> Model item location character knowledge
    -> location
getCurrentLocation world (Model storyState) =
    Engine.State.getCurrentLocation <| buildStoryState world storyState


{-| Get a list of the items in the current location to display
-}
getNearByProps :
    World item location character
    -> Model item location character knowledge
    -> List item
getNearByProps world (Model storyState) =
    Engine.State.getItemsInCurrentLocation <| buildStoryState world storyState


{-| Get a list of the characters in the current location to display
-}
getNearByCharacters :
    World item location character
    -> Model item location character knowledge
    -> List character
getNearByCharacters world (Model storyState) =
    Engine.State.getCharactersInCurrentLocation <| buildStoryState world storyState


{-| Get a list of the items in your inventory to display
-}
getInventory :
    World item location character
    -> Model item location character knowledge
    -> List item
getInventory world (Model storyState) =
    Engine.State.getInventory <| buildStoryState world storyState


{-| Get a list of the known locations to display
-}
getLocations :
    World item location character
    -> Model item location character knowledge
    -> List location
getLocations world (Model storyState) =
    Engine.State.getLocations <| buildStoryState world storyState


{-| Get the story revealed so far as a list of narration items.
-}
getStoryLine :
    World item location character
    -> Model item location character knowledge
    -> List ( String, String )
getStoryLine world (Model storyState) =
    Engine.State.getStoryLine <| buildStoryState world storyState


{-| A declarative rule, describing how to advance your story and under what conditions.
-}
type alias Rule item location character knowledge =
    Types.Rule item location character knowledge


{-| -}
type alias InteractionMatcher item location character =
    Types.InteractionMatcher item location character


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified item.
-}
withItem : item -> InteractionMatcher item location character
withItem item =
    WithItem item


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified location.
-}
withLocation : location -> InteractionMatcher item location character
withLocation location =
    WithLocation location


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified character.
-}
withCharacter : character -> InteractionMatcher item location character
withCharacter character =
    WithCharacter character


{-| Will match the `interaction` part of a story rule if the player interacted with any item (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyItem : InteractionMatcher item location character
withAnyItem =
    WithAnyItem


{-| Will match the `interaction` part of a story rule if the player interacted with any location (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyLocation : InteractionMatcher item location character
withAnyLocation =
    WithAnyLocation


{-| Will match the `interaction` part of a story rule if the player interacted with any character (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyCharacter : InteractionMatcher item location character
withAnyCharacter =
    WithAnyCharacter


{-| Will match the `interaction` part of a story rule every time (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnything : InteractionMatcher item location character
withAnything =
    WithAnything


{-| -}
type alias Condition item location character knowledge =
    Types.Condition item location character knowledge


{-| Will only match if the supplied item is in the inventory.
-}
withInventory : item -> Condition item location character knowledge
withInventory =
    WithInventory


{-| Will only match if the supplied character in in the current location.
-}
nearCharacter : character -> Condition item location character knowledge
nearCharacter =
    NearCharacter


{-| Will only match if the supplied item is in the current location.  Ignores inventory.
-}
nearItem : item -> Condition item location character knowledge
nearItem =
    NearItem


{-| Will only match when the supplied location is the current location.
-}
inLocation : location -> Condition item location character knowledge
inLocation =
    InLocation


{-| Will only match if the specified knowledge has been acquired.
-}
withKnowledge : knowledge -> Condition item location character knowledge
withKnowledge =
    WithKnowledge


{-| Will only match if the supplied condition does NOT match.
-}
unless : Condition item location character knowledge -> Condition item location character knowledge
unless =
    Unless


{-| -}
type alias ChangeWorldCommand item location character knowledge =
    Types.ChangeWorldCommand item location character knowledge


{-| Changes the current location.  The current location will be highlighted in the list of known locations, and will also be displayed at the top of the page, highlighted in the color defined for that location.  Any items or characters that are in the current location will also be shown for the player to interact with.
-}
moveTo : location -> ChangeWorldCommand item location character knowledge
moveTo =
    MoveTo


{-| Adds a location to your list of known locations.  Any location on this list is available for the player to click on at any time.  This avoids clunky spatial navigation mechanics, but does mean that you will need to make rules to prevent against going to locations that are inaccessible (with appropriate narration).
-}
addLocation : location -> ChangeWorldCommand item location character knowledge
addLocation =
    AddLocation


{-| Removes a location from your list of known locations.  You probably don't need this since once you know about a location you would always know about it, and trying to go to a location that is inaccessible for some reason could just give some narration telling why.  But maybe you will find a good reason to use it.
-}
removeLocation : location -> ChangeWorldCommand item location character knowledge
removeLocation =
    RemoveLocation


{-| Adds an item to your inventory (if it was previously in a location, it will be removed from there, as items can only be in one place at once).
-}
addInventory : item -> ChangeWorldCommand item location character knowledge
addInventory =
    AddInventory


{-| Removes an item from your inventory.  The item will not show up anywhere until you add it back to your inventory or to a location.
-}
removeInventory : item -> ChangeWorldCommand item location character knowledge
removeInventory =
    RemoveInventory


{-| Adds a character to a location, or moves a character to a different location (characters can only be in one location at a time, or off-screen).  (Use moveTo to move yourself between locations.)
-}
moveCharacter : character -> location -> ChangeWorldCommand item location character knowledge
moveCharacter =
    MoveCharacter


{-| Moves a character "off-screen".  The character will not show up in any locations until you use `moveCharacter` again.
-}
removeCharacter : character -> ChangeWorldCommand item location character knowledge
removeCharacter =
    RemoveCharacter


{-| Move an item to a location.  If it was in another location or your inventory before, it will remove it from there, as items can only be in one place at once.
-}
placeItem : item -> location -> ChangeWorldCommand item location character knowledge
placeItem =
    PlaceItem


{-| Moves an item "off-screen" (either from a location or the inventory).  The item will not show up in any locations or inventory until you use `placeItem` or `addInventory` again.
-}
removeItem : item -> ChangeWorldCommand item location character knowledge
removeItem =
    RemoveItem


{-| Knowledge is an intangible "flag" that you can match against in your rules.  For example if you add knowledge of learning about a suspect, then going back to people you have already interacted with can give you new information about the suspect when you interact with them again.  You can also use this for acquiring skills or bonuses or anything intangible that would not be displayed in the story.  You could track your actions, such as if you were kind or mean to an important character in an earlier scene.

However, before turning to this tool, consider if you can use a normal, story interactable instead.  For example, perhaps you get a sketch of the suspect in your inventory, which you can "show" to people for more information.  This keeps the story more concrete.

    type MyKnowledge = LearnOfSuspect | WrongSuspect | Amnesia

    addKnowledge LearnOfSuspect
-}
addKnowledge : knowledge -> ChangeWorldCommand item location character knowledge
addKnowledge =
    AddKnowledge


{-| Rules are grouped into "scenes" for better organization and control.  This is how you switch between scenes when you want a different rule set.  You may want to switch scenes at a "turning point" in your story to bring about new rules for the next objective.

    scene1 = [...rules here...]
    scene2 = [...rules here...]

    -- in the `changes` part of a rule in a scene1:
    loadScene scene2
-}
loadScene : List (Rule item location character knowledge) -> ChangeWorldCommand item location character knowledge
loadScene =
    LoadScene


{-| Let the framework know the story has ended.  Currently this has no effect, I'm trying to figure out what should happen when stories end.  You may want to make the rule load a special "storyFinished" scene that only has one `withAnything` interaction matcher so the player can't do anything else (unless you want to let them "wander").
-}
endStory : ChangeWorldCommand item location character knowledge
endStory =
    EndStory


{-| The update function you'll need if embedding the engine in your own app to progress the `Model`
-}
update :
    Msg item location character
    -> Model item location character knowledge
    -> Model item location character knowledge
update msg (Model storyHistory) =
    case msg of
        NoOp ->
            Model storyHistory

        Interact interactable ->
            Model { storyHistory | interactions = storyHistory.interactions ++ [ Interaction interactable ] }

        Rollback i ->
            Model { storyHistory | interactions = List.take i storyHistory.interactions }
