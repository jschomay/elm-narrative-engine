module Engine
    exposing
        ( Model
        , init
        , update
        , changeWorld
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
        , with
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

{-|
The story engine handles storing and advancing the state of the "world model" by running through your story rules on each interaction and updating the world model appropriately.  It is designed to be embedded in your own Elm app, allowing for maximum flexibility and customization.

You can base your app on the [interactive story starter repo](https://github.com/jschomay/elm-interactive-story-starter.git).

@docs Model

## Embedding the engine

@docs init, changeWorld, chooseFrom

## Accessors

The engine exposes many accessor functions for each part of the story world.  Note that each of these only return ids.  It is up to the client to map an id to the appropriate associated display content.  It is useful to follow an "Entity Component System" pattern in the client for this purpose (see the story starter).

@docs  getCurrentScene, getCurrentLocation, getItemsInCurrentLocation, getCharactersInCurrentLocation, getItemsInInventory, getLocations, getEnding


## Story rules

Rules are how you progress the story.  They are made up of conditions to match against and commands to perform if the rule matches.  On each call of `update`, the engine will run through all of the rules to find the best match.  If no rules match, the framework will perform a default command, which is usually just to narrate the description of what was interacted with, or to move you to that location or take that item.  If multiple rules match the current state of the world, the "best" choice will be selected based on the following weighting criteria:

1. a `currentSceneIs` condition has the highest weight
2. `with` has more weight than the broader `withAny*` matchers
3. each additional condition adds more weight

In the case of a tie, the first candidate will be chosen, which is something you want to avoid, so design your rules carefully


# Anatomy of a rule

1. A matcher against what interactable story element id the user clicked on
2. A list of conditions that all must match for the rule to match
3. A list of changes to make if the rule matches

        rules =
           [ { interaction = with "River"
             , conditions =
                  [ currentLocationIs "Cottage"
                  , itemIsInInventory "Cape"
                  , itemIsInInventory "Basket of food"
                  ]
             , changes =
                  [ moveTo "River"
                  , moveCharacterToLocation "Little Red Riding Hood" "River"
                  ]
             }
          -- etc
          ]


@docs Rule, Rules, update

### Interaction matchers

The following interaction matchers can be used in the `interaction` part of the rule record.

@docs InteractionMatcher, with, withAnything, withAnyItem, withAnyLocation, withAnyCharacter


### Conditions

The following condition matchers can be used in the `conditions` part of the rule record.

@docs  Condition, itemIsInInventory , characterIsInLocation , itemIsInLocation , currentLocationIs, itemIsNotInInventory , hasPreviouslyInteractedWith, hasNotPreviouslyInteractedWith, currentSceneIs, characterIsNotInLocation , itemIsNotInLocation , currentLocationIsNot


### Changing the story world

You cannot change the story directly, but you can supply "commands" describing how the story state should change.

@docs ChangeWorldCommand, moveTo, addLocation, removeLocation, moveItemToInventory, moveCharacterToLocation, moveCharacterOffScreen, moveItemToLocation, moveItemToLocationFixed, moveItemOffScreen, loadScene, endStory

-}

import Types exposing (..)
import Engine.Manifest exposing (..)
import Engine.Rules exposing (..)


{-| The opaque type that holds all of the "world model" state, such as where each item and character is, what the current location and scene are, etc.
-}
type Model
    = Model Types.Story


{-| Initialize the `Model` for use when embedding in your own app.  Provide your "manifest" (a list of ids) of all of the items, characters, and locations in your story, and the rules that govern the story.

You will most likely want to call `changeWorld` immediately after `init` to setup your initial story state (the current scene, location, and any initial placements of items or characters and known locations).
-}
init :
    { items : List String
    , locations : List String
    , characters : List String
    }
    -> Rules
    -> Model
init manifest rules =
    Model
        { history = []
        , manifest = Engine.Manifest.init manifest
        , rules = rules
        , currentScene = ""
        , currentLocation = ""
        , theEnd = Nothing
        }


{-| This gets the current scene to display
-}
getCurrentScene : Model -> String
getCurrentScene (Model story) =
    story.currentScene


{-| Get the current location to display
-}
getCurrentLocation : Model -> String
getCurrentLocation (Model story) =
    story.currentLocation


{-| Get a list of the items in the current location to display
-}
getItemsInCurrentLocation : Model -> List String
getItemsInCurrentLocation (Model story) =
    Engine.Manifest.getItemsInLocation story.currentLocation story.manifest


{-| Get a list of the characters in the current location to display
-}
getCharactersInCurrentLocation : Model -> List String
getCharactersInCurrentLocation (Model story) =
    Engine.Manifest.getCharactersInLocation story.currentLocation story.manifest


{-| Get a list of the items in your inventory to display
-}
getItemsInInventory : Model -> List String
getItemsInInventory (Model story) =
    Engine.Manifest.getItemsInInventory story.manifest


{-| Get a list of the known locations to display
-}
getLocations : Model -> List String
getLocations (Model story) =
    Engine.Manifest.getLocations story.manifest


{-| Get the story ending, if it has ended.  (Set with `EndStory`)
-}
getEnding : Model -> Maybe String
getEnding (Model story) =
    story.theEnd


{-| This is how you progress the story.  Call it with the id of what ever was just "interacted" with.  This will apply the best matching rule for the current context, or if it does not find a matching rule, it will perform some sensible default changes such as adding an item to inventory or moving to a location if no rules match.  It also adds the interaction to the history (which is used for `hasPreviouslyInteractedWith`, save/load, and undo).

This will also return the id of the matching rule (if there was one).  Normally the client would look up some associated narrative by this id to display, though it could respond in any other way as well.
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
        ( changeWorld changes model |> addHistory
        , Maybe.map Tuple.first matchingRule
        )


{-| A way to change the story world directly, rather than responding to a player's interaction.

For example, you could change the current location in the story based on browser geolocation events, or respond to a network event, etc.  This is also used to set up any initial story state.

If you are simply responding to a player's interaction, use `update` instead.
-}
changeWorld :
    List ChangeWorldCommand
    -> Model
    -> Model
changeWorld changes (Model story) =
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

This uses the same weighting scale as the rules (below), and likewise will return the first match if there is a tie, so specify your conditions carefully.
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


{-| Will only match the `interaction` part of a story rule if the player interacted with the specified entity id.
-}
with : String -> InteractionMatcher
with id =
    With id


{-| Will match the `interaction` part of a story rule if the player interacted with any item (be careful about the conditions of your rules since this matcher is so broad).
-}
withAnyItem : InteractionMatcher
withAnyItem =
    WithAnyItem


{-| Will match the `interaction` part of a story rule if the player interacted with any location (be careful about the conditions of your rules since this matcher is so broad).
-}
withAnyLocation : InteractionMatcher
withAnyLocation =
    WithAnyLocation


{-| Will match the `interaction` part of a story rule if the player interacted with any character (be careful about the conditions of your rules since this matcher is so broad).
-}
withAnyCharacter : InteractionMatcher
withAnyCharacter =
    WithAnyCharacter


{-| Will match the `interaction` part of a story rule every time (be careful about the conditions of your rules since this matcher is so broad).
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


{-| Will only match if the supplied character is *not* in the supplied location.

The first String is a character id, the second is a location id.
-}
characterIsNotInLocation : String -> String -> Condition
characterIsNotInLocation =
    CharacterIsNotInLocation


{-| Will only match if the supplied item is in the supplied location.

The first String is a item id, the second is a location id.
-}
itemIsInLocation : String -> String -> Condition
itemIsInLocation =
    ItemIsInLocation


{-| Will only match if the supplied item is *not* in the supplied location.

The first String is a item id, the second is a location id.
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


{-| Changes the current location.
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
-}
moveItemToLocationFixed : String -> String -> ChangeWorldCommand
moveItemToLocationFixed =
    MoveItemToLocationFixed


{-| Move an item to a location.  If it was in another location or your inventory before, it will remove it from there, as items can only be in one place at once.

The first String is an item id, the second is a location id.
-}
moveItemToLocation : String -> String -> ChangeWorldCommand
moveItemToLocation =
    MoveItemToLocation


{-| Moves an item "off-screen" (either from a location or the inventory).  The item will not show up in any locations or inventory until you use `placeItem` or `addInventory` again.
-}
moveItemOffScreen : String -> ChangeWorldCommand
moveItemOffScreen =
    MoveItemOffScreen


{-| Scenes are a way to further constrain rules.  You could have a scene for each leg of your story to make sure only the rules for that scene will apply.  Or you may start a scene at a turning point in your story to "activate" special rules that apply to that scene.  This is how you start or switch to a new scene.  Note that you can only have one scene active at at time.
-}
loadScene : String -> ChangeWorldCommand
loadScene =
    LoadScene


{-| Sets a flag that the story has ended.  The string you provide can be used to signify the "type" of story ending ("good", "bad", "heroic", etc), or how many moves it took to complete, or anything else you like.  This has no effect on the framework, but you can use it in your client code how ever you like (change the view, calculate a score, etc).
-}
endStory : String -> ChangeWorldCommand
endStory ending =
    EndStory ending
