# Changelog

Follow along with development on the [developement blog](http://blog.elmnarrativeengine.com/).

## 3.0.0

Many breaking changes this time around breaking the view layer out of the story engine.  Now, instead of loading your "story config" into the engine, you embed the engine in your own app, and provide your own view layer (with the help of many new accessor functions).  See the [interactive story starter repo](https://github.com/jschomay/elm-interactive-story-starter.git) to see how that works.

Also, this release changes the top level module namespace from `Story` to `Engine`.

This release also upgrades to Elm 0.18 behind the scenes.

### Changes

- removed `load` and added `init` and `update` for use in your own app's model and update functions
- exposed opaque types `Engine.Model` and `Engine.Msg` for the type signatures in your app
- added functions to generate `Engine.Msg` messages:
  - `itemMsg`
  - `locationMsg`
  - `characterMsg`
  - `rollbackMsg`
- added many accessor functions:
  - `getCurrentLocation`
  - `getInventory`
  - `getLocations`
  - `getNearByCharacters`
  - `getNearByProps`
  - `getStoryLine`


## 2.0.0

This version adds some new features, fixes some bugs, and changes the public api significantly for design reasons (partially leading towards future versions).

### Changes

- The format of story rules has changed significantly.  Instead of the infixed rule-building DSL of version 1.0.0 (`interactingWith`, `firstInteractionWith`, `everyTime`, `when`, `changesWorld`, `narrates`, `item`, `location` and `character`), rules now are defined as records:

      scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
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
