# Changelog

Follow along with development on the [development blog](http://blog.elmnarrativeengine.com/).

## 4.0.0

100% breaking changes!  Huge shifts in both api and concepts:

- shift to full salience and property based systems!
- shift to moving most functionality external to engine
- removed everything specific to "items", "locations", "characters", "scenes", "ending", "history"
- replaced with "tags", "stats", "links" for clients to build up their own semantic requirements
- only controls `WorldModel` and `Rules` data, instead of controlling the whole model

Exported modules change:

- No more `Engine`
- Added `Narrative.WorldModel`
- Added `Narrative.Rules`

Other

- added example
- removed dep on `elm/html` since it isn't used

## 3.0.1

Update for Elm 0.19!

No api changes


## 3.0.0

This version is a major architectural shift from previous versions, allowing for extreme flexibility.  Some notable changes include:

- updated to Elm 0.18
- top-level module namespace changed to `Engine`
- the view layer has been completely removed from the engine.  The engine solely handles maintaining the story world state by matching interactions against rule sets.
- exposes many accessor functions for the client to use as needed
- rules are now matched based on a weighting scale to pick a winner when multiple match
- some new/different matchers and change world commands
- replaced `withItem`/`withCharacter`/`withLocation` with simply `with`, as they all have the same signature
- ids are now Strings instead of types, allowing for dynamic generation and serialization, and also fitting well with the Entity/Component/System pattern
- `changeWold` function to directly alter the story world
- `chooseFrom` function to encode conditional choices in data
- scenes are not just another condition rather than a grouping of rules


## 2.0.0

This version adds some new features, fixes some bugs, and changes the public api significantly for design reasons (partially leading towards future versions).

### Changes

- The format of story rules has changed significantly.  Instead of the infixed rule-building DSL of version 1.0.0 (`interactingWith`, `firstInteractionWith`, `everyTime`, `when`, `changesWorld`, `narrates`, `item`, `location` and `character`), rules now are defined as records:

      scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
      scene1 =
          [ { interaction = withCharacter Harry
            , conditions = [ currentLocationIs Garden ]
            , changes = [ moveCharacter Harry Marsh, addInventory NoteFromHarry ]
            , narration = [ "He gives you a note, then runs off.", "I wonder what he wants?" ]
            }
          , { interaction = withInventory NoteFromHarry
            , conditions = []
            , changes = [ addLocation Marsh ]
            , narration = [ "It says, \"*Meet me in the marsh.*\"" ]
            }
          ]


- `MyScenes` type has been removed in favor of linking directly to your list of rules
- Items and characters can now only be in one place at a time.  This means `addCharacter` and `addItem` were removed with `moveCharacter` and `placeItem` taking their place.  Also `removeCharacter` and `removeItem` only take a single argument now.
- Interacting with a location now moves you there by default.
- `withItem` now is an `InteractionMatcher` instead of a `ChangeWorldCommand`.  Use `withInventory` instead.
- `nearProp` and `addProp` and `removeProp` are now `nearItem`, `placeItem` and `removeItem`.
- `all` and `any` condition matchers removed.
- `storyWorld` is now `world` and `setup` is now `startingState`.
- `Element` is now `Interactable`

### New features

- Progressive narration - the narration field of a rule now takes a list of strings.  If a player clicks on the same story element multiple times, the engine will loop through each item in the list, repeating the final item.  This allows for a deeper story texture and more variety in narration.
- Story rollback - the internal state of the story has changed to allow for "rolling back" to an earlier point in the story to try a different direction.
- New "broad-scope" interaction matchers: `withAnyItem`, `withAnyLocation`, `withAnyCharacter` and `withAnything`.

## 1.0.0

[Initial release](http://package.elm-lang.org/packages/jschomay/elm-narrative-engine/1.0.0).  Sample story [source code](https://github.com/jschomay/elm-interactive-story-starter/tree/a481a0d8a2662fe1b08a2cffff0334c9c1b74dec/src) and [playable link](http://blog.elmnarrativeengine.com/sample-stories/curse-of-the-tech-demo/).  See [demonstration video from ElmConf](http://youtube.com/watch?v=t8RSxzpw1Yw)
