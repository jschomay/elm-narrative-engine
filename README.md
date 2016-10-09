# Elm Narrative Engine

A framework for telling interactive stories, based on specific design principles:

For the readers/players:

* No command line interface (having to type "go north" or "inspect umbrella"), just simple point-and-click
* More interaction and agency than simply choosing which branch to follow
* Avoid clunky mechanics such as spacial navigation and object manipulation, and instead focus on the story

For authors:

* Simple to use - focus on the story, not on the code
* Declarative, data-driven approach - describe your story world and how it changes over time, the framework will take care of rest

## Getting Started

See https://github.com/jschomay/elm-interactive-story-starter.git for everything you need to get started, including an example story you can modify with your own content.

## Basic Usage

Step 1: Define your "story displayables" (items, locations, and characters)

```elm
type MyItem
    = Umbrella
    ...


items : MyItem -> ItemInfo
items item =
    case item of
        Umbrella ->
            itemInfo "Umbrella" "Your trusty umbrella, you bring it everywhere with you."
    ...
```

Step 2: Define your declarative "story rules," broken up into "scenes"

```elm
scene1 : List (Story.Rule MyItem MyLocation MyCharacter MyKnowledge)
scene1 =
    [ { interaction = character Harry
      , conditions = [ inLocation Garden ]
      , changes = [ addCharacter Harry Marsh, removeCharacter Harry Garden ]
      , narration = "Meet me in the marsh..."
      }
    , { interaction = character Harry
      , conditions = [ inLocation Marsh ]
      , changes = []
      , narration = "My good friend Harry..."
      }
    ]
```

Step 3: Load your story displayables and rules into the framework

```elm
Story.load info displayables setup
```

## Sample Stories

* coming soon...
