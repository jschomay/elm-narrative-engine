module Engine
    exposing
        ( Model
        , init
        , update
        , chooseFrom
        , getCurrentScene
        , getCurrentLocation
        , getItemsInCurrentLocation
        , getCharactersInCurrentLocation
        , getItemsInInventory
        , getLocations
        , getEnding
        , Rules
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
        , characterIsNotInLocation
        , characterIsInLocation
        , currentLocationIs
        , currentLocationIsNot
        , itemIsInInventory
        , itemIsNotInInventory
        , itemIsNotInLocation
        , itemIsInLocation
        , hasPreviouslyInteractedWith
        , hasNotPreviouslyInteractedWith
        , currentSceneIs
        , ChangeWorldCommand
        , addLocation
        , endStory
        , loadScene
        , moveCharacterToLocation
        , moveCharacterOffScreen
        , moveItemToLocation
        , moveItemToLocationFixed
        , moveItemOffScreen
        , moveItemToInventory
        , moveTo
        , removeLocation
        )

{-| The story engine handles storing and advancing your story state by running through your story rules on each interaction.  It allows the client code to handle building the story world, story rules, and display layer.

# Embedding the story engine

The story engine is designed to be embedded in your own Elm app, allowing for maximum flexibility and customization.

You can base your app on the [interactive story starter repo](https://github.com/jschomay/elm-interactive-story-starter.git).

@docs Model, init, update, chooseFrom, getCurrentScene, getCurrentLocation, getItemsInCurrentLocation, getCharactersInCurrentLocation, getItemsInInventory, getLocations, getEnding

# Defining your story world

TODO

# Defining story rules

Rules are how you progress the story.  They are made up of conditions to match against and commands to perform if the rule matches.  Rules are grouped into "scenes" for better control and organization.  The engine will run through the active scene from the beginning, looking for the first matching rule, then run it.  If no rules match, the framework will perform a default command, which is usually just to narrate the description of what was interacted with, or to move you to that location or take that item.


A rule has four parts:

1. A matcher against what interactable story element the user clicked on
2. A list of conditions that all must match for the rule to match
3. A list of changes to make if the rule matches
4. Narration to add to the story line if the rule matches (note that you can use markdown)

TODO - UPDATE THIS!!!!!!!!!!

    scene1 : List Engine.Rule
    scene1 =
        [ { interaction = withCharacter Harry
          , conditions = [ currentLocationIs Garden ]
          , changes = [ moveCharacterToLocation Harry Marsh, addInventory NoteFromHarry ]
          , narration = [ "He gives you a note, then runs off.", "I wonder what he wants?" ]
          }
        , { interaction = itemIsInInventory NoteFromHarry
          , conditions = []
          , changes = [ addLocation Marsh ]
          , narration = [ "It says, \"*Meet me in the marsh.*\"" ]
          }
        ]

When a rule matches multiple times (a player clicks the same story element multiple times), it will run through the list of narrations in order, one per click, repeating the final one when it reaches the end.

@docs Rule, Rules

## Interaction matchers

The following interaction matchers can be used in the `interaction` part of the rule record.

@docs InteractionMatcher, withItem, withLocation, withCharacter, withAnything, withAnyItem, withAnyLocation, withAnyCharacter


## Conditions

The following condition matchers can be used in the `conditions` part of the rule record.

@docs  Condition, itemIsInInventory , characterIsInLocation , itemIsInLocation , currentLocationIs, itemIsNotInInventory , hasPreviouslyInteractedWith, hasNotPreviouslyInteractedWith, currentSceneIs, characterIsNotInLocation , itemIsNotInLocation , currentLocationIsNot


## Changing the story world

You cannot change the story directly, but you can supply "commands" describing how the story state should change.

@docs ChangeWorldCommand, moveTo, addLocation, removeLocation, moveItemToInventory, moveCharacterToLocation, moveCharacterOffScreen, moveItemToLocation, moveItemToLocationFixed, moveItemOffScreen, loadScene, endStory

-}

import Types exposing (..)
import Engine.Manifest exposing (..)
import Engine.Rules exposing (..)


{-| A interactable story element -- and item, location, or character in your story that can be displayed and interacted with.
-}
type alias Interactable =
    Types.Interactable


{-| You'll need this type if you embed the engine in your own app.
-}
type Model
    = Model Types.Story


{-| Initialize the `Model` for use when embedding in your own app.
-}
init :
    { manifest :
        { items : List String
        , locations : List String
        , characters : List String
        }
    , rules : Rules
    , startingScene : String
    , startingLocation : String
    , setup : List ChangeWorldCommand
    }
    -> Model
init { manifest, rules, startingScene, startingLocation, setup } =
    Model
        { history = []
        , manifest = Engine.Manifest.init manifest
        , rules = rules
        , currentScene = startingScene
        , currentLocation = startingLocation
        , theEnd = Nothing
        }
        |> update_ setup


{-| Get the current sceen to display
-}
getCurrentScene :
    Model
    -> String
getCurrentScene (Model story) =
    story.currentScene


{-| Get the current location to display
-}
getCurrentLocation :
    Model
    -> String
getCurrentLocation (Model story) =
    story.currentLocation


{-| Get a list of the items in the current location to display
-}
getItemsInCurrentLocation :
    Model
    -> List String
getItemsInCurrentLocation (Model story) =
    Engine.Manifest.getItemsInLocation story.currentLocation story.manifest


{-| Get a list of the characters in the current location to display
-}
getCharactersInCurrentLocation :
    Model
    -> List String
getCharactersInCurrentLocation (Model story) =
    Engine.Manifest.getCharactersInLocation story.currentLocation story.manifest


{-| Get a list of the items in your inventory to display
-}
getItemsInInventory :
    Model
    -> List String
getItemsInInventory (Model story) =
    Engine.Manifest.getItemsInInventory story.manifest


{-| Get a list of the known locations to display
-}
getLocations :
    Model
    -> List String
getLocations (Model story) =
    Engine.Manifest.getLocations story.manifest


{-| Get the story ending, if it has ended.  (Set with `EndStory`)
-}
getEnding : Model -> Maybe String
getEnding (Model story) =
    story.theEnd


{-| The update function you'll need if embedding the engine in your own app to progress the `Model`.

Returns the updated Engine.Model along with the id of the matching rule (if any).
-}
update :
    String
    -> Model
    -> ( Model, Maybe String )
update interactableId ((Model story) as model) =
    let
        defaultChanges : List ChangeWorldCommand
        defaultChanges =
            if Engine.Manifest.isLocation interactableId story.manifest then
                [ MoveTo interactableId ]
            else if Engine.Manifest.isItem interactableId story.manifest then
                [ MoveItemToInventory interactableId ]
            else
                []

        matchingRule : Maybe ( String, Rule )
        matchingRule =
            findMatchingRule story interactableId

        changes : List ChangeWorldCommand
        changes =
            matchingRule
                |> Maybe.map (Tuple.second >> .changes)
                |> Maybe.withDefault defaultChanges

        addHistory : Model -> Model
        addHistory (Model story) =
            Model <| { story | history = story.history ++ [ interactableId ] }
    in
        ( update_ changes model |> addHistory
        , Maybe.map Tuple.first matchingRule
        )


update_ :
    List ChangeWorldCommand
    -> Model
    -> Model
update_ changes (Model story) =
    let
        doChange change story =
            case change of
                MoveTo location ->
                    { story | currentLocation = location }

                LoadScene sceneName ->
                    { story | currentScene = sceneName }

                EndStory ending ->
                    { story | theEnd = Just ending }

                _ ->
                    { story
                        | manifest = Engine.Manifest.update change story.manifest
                    }
    in
        List.foldr doChange story changes
            |> Model


{-| Given a list of choices, this will return only the choice that matches the associated conditions, if any.  Useful for conditional descriptions, for example, where an item has a different description depending on where it is.
-}
chooseFrom : Model -> List { a | conditions : List Condition } -> Maybe { a | conditions : List Condition }
chooseFrom (Model story) choices =
    Engine.Rules.chooseFrom story choices


{-| A declarative rule, describing how to advance your story and under what conditions.
-}
type alias Rule =
    Types.Rule


{-| All the rules in your story.
-}
type alias Rules =
    Types.Rules


{-| -}
type alias InteractionMatcher =
    Types.InteractionMatcher


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified item.
-}
withItem : String -> InteractionMatcher
withItem item =
    WithItem item


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified location.
-}
withLocation : String -> InteractionMatcher
withLocation location =
    WithLocation location


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified character.
-}
withCharacter : String -> InteractionMatcher
withCharacter character =
    WithCharacter character


{-| Will match the `interaction` part of a story rule if the player interacted with any item (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyItem : InteractionMatcher
withAnyItem =
    WithAnyItem


{-| Will match the `interaction` part of a story rule if the player interacted with any location (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyLocation : InteractionMatcher
withAnyLocation =
    WithAnyLocation


{-| Will match the `interaction` part of a story rule if the player interacted with any character (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnyCharacter : InteractionMatcher
withAnyCharacter =
    WithAnyCharacter


{-| Will match the `interaction` part of a story rule every time (be careful about the the order and conditions of your rules since this matcher is so broad).
-}
withAnything : InteractionMatcher
withAnything =
    WithAnything


{-| -}
type alias Condition =
    Types.Condition


{-| Will only match if the supplied item is in the inventory.
-}
itemIsInInventory : String -> Condition
itemIsInInventory =
    ItemIsInInventory


{-| Will only match if the supplied item is *not* in the inventory.
-}
itemIsNotInInventory : String -> Condition
itemIsNotInInventory =
    ItemIsNotInInventory


{-| Will only match if the supplied character is in the supplied location.

The first String is a character id, the second is a location id.

    characterIsInLocation "Harry" "Marsh"
-}
characterIsInLocation : String -> String -> Condition
characterIsInLocation =
    CharacterIsInLocation


{-| Will only match if the supplied interactable has already been interacted with.
-}
hasPreviouslyInteractedWith : String -> Condition
hasPreviouslyInteractedWith =
    HasPreviouslyInteractedWith


{-| Will only match if the supplied interactable has not already been interacted with.
-}
hasNotPreviouslyInteractedWith : String -> Condition
hasNotPreviouslyInteractedWith =
    HasNotPreviouslyInteractedWith


{-| Will only match if the supplied character is not in the supplied location.

The first String is a character id, the second is a location id.

    characterIsNotInLocation "Harry" "Marsh"
-}
characterIsNotInLocation : String -> String -> Condition
characterIsNotInLocation =
    CharacterIsNotInLocation


{-| Will only match if the supplied item is in the supplied location.

The first String is a item id, the second is a location id.

    itemIsInLocation "Umbrella" "Marsh"
-}
itemIsInLocation : String -> String -> Condition
itemIsInLocation =
    ItemIsInLocation


{-| Will only match if the supplied item is not in the supplied location.

The first String is a item id, the second is a location id.

    itemIsNotInLocation "Umbrella" "Marsh"
-}
itemIsNotInLocation : String -> String -> Condition
itemIsNotInLocation =
    ItemIsNotInLocation


{-| Will only match when the supplied location is the current location.
-}
currentLocationIs : String -> Condition
currentLocationIs =
    CurrentLocationIs


{-| Will only match when the supplied location is *not* the current location.
-}
currentLocationIsNot : String -> Condition
currentLocationIsNot =
    CurrentLocationIsNot


{-| Will only match when the supplied location is *not* the current location.
-}
currentSceneIs : String -> Condition
currentSceneIs =
    CurrentSceneIs


{-| -}
type alias ChangeWorldCommand =
    Types.ChangeWorldCommand


{-| Changes the current location.  The current location will be highlighted in the list of known locations, and will also be displayed at the top of the page, highlighted in the color defined for that location.  Any items or characters that are in the current location will also be shown for the player to interact with.
-}
moveTo : String -> ChangeWorldCommand
moveTo =
    MoveTo


{-| Adds a location to your list of known locations.  Any location on this list is available for the player to click on at any time.  This avoids clunky spatial navigation mechanics, but does mean that you will need to make rules to prevent against going to locations that are inaccessible (with appropriate narration).
-}
addLocation : String -> ChangeWorldCommand
addLocation =
    AddLocation


{-| Removes a location from your list of known locations.  You probably don't need this since once you know about a location you would always know about it, and trying to go to a location that is inaccessible for some reason could just give some narration telling why.  But maybe you will find a good reason to use it.
-}
removeLocation : String -> ChangeWorldCommand
removeLocation =
    RemoveLocation


{-| Adds an item to your inventory (if it was previously in a location, it will be removed from there, as items can only be in one place at once).  If the item is "fixed" this will not move it (if you want to "unfix" an item, use `moveItemOffScreen` or `MoveItemToLocation` first).

-}
moveItemToInventory : String -> ChangeWorldCommand
moveItemToInventory =
    MoveItemToInventory


{-| Adds a character to a location, or moves a character to a different location (characters can only be in one location at a time, or off-screen).  (Use moveTo to move yourself between locations.)

The first String is a character id, the second is a location id.

    moveCharacterToLocation "Harry" "Marsh"
-}
moveCharacterToLocation : String -> String -> ChangeWorldCommand
moveCharacterToLocation =
    MoveCharacterToLocation


{-| Moves a character "off-screen".  The character will not show up in any locations until you use `moveCharacterToLocation` again.
-}
moveCharacterOffScreen : String -> ChangeWorldCommand
moveCharacterOffScreen =
    MoveCharacterOffScreen


{-| Move an item to a location and set it as "fixed."  Fixed items are like scenery, they can be interacted with, but they cannot be added to inventory.

If it was in another location or your inventory before, it will remove it from there, as items can only be in one place at once.

The first String is an item id, the second is a location id.

    MoveItemToLocationFixed "Umbrella" "Marsh"
-}
moveItemToLocationFixed : String -> String -> ChangeWorldCommand
moveItemToLocationFixed =
    MoveItemToLocationFixed


{-| Move an item to a location.  If it was in another location or your inventory before, it will remove it from there, as items can only be in one place at once.

The first String is an item id, the second is a location id.

    MoveItemToLocation "Umbrella" "Marsh"
-}
moveItemToLocation : String -> String -> ChangeWorldCommand
moveItemToLocation =
    MoveItemToLocation


{-| Moves an item "off-screen" (either from a location or the inventory).  The item will not show up in any locations or inventory until you use `placeItem` or `addInventory` again.
-}
moveItemOffScreen : String -> ChangeWorldCommand
moveItemOffScreen =
    MoveItemOffScreen


{-| Rules are grouped into "scenes" for better organization and control.  This is how you switch between scenes when you want a different rule set.  You may want to switch scenes at a "turning point" in your story to bring about new rules for the next objective.

    scene1 = [...rules here...]
    scene2 = [...rules here...]

    -- in the `changes` part of a rule in a scene1:
    loadScene scene2
-}
loadScene : String -> ChangeWorldCommand
loadScene =
    LoadScene


{-| Sets a flag that the story has ended.  The string you provide can be used to signify the "type" of story ending ("good", "bad", "heroic", etc), or how many moves it took to complete, or anything else you like.  This has no effect on the framework, but you can use it in your client code how ever you like (change the view, calculate a score, etc).
-}
endStory : String -> ChangeWorldCommand
endStory ending =
    EndStory ending
