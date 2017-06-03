# Elm Narrative Engine

A unique tool for telling interactive stories.

## Features

- Context-based rule-matching system for immersive and responsive stories
- Extremely flexible and extensible, see the sample stories below for examples!
- Total separation of logic, presentation, and content
- Data-driven and declarative
- Designed to work with the Entity Component System pattern
- Possible to integrate with other tools including visual editors and other game frameworks

## Sample Stories

[Play sample stories made with the Elm Narrative Engine](http://blog.elmnarrativeengine.com/sample-stories/)

## Getting Started

The Elm Narrative Engine is written in the [Elm language](http://elm-lang.org), and is intended to be embedded in an Elm client app.  If you are a developer, this allows for great customization.

You can also get started by cloning https://github.com/jschomay/elm-interactive-story-starter.git as a template that you can modify to get going on your own story.

The main story design happens by designing your rule sets, which look similar to this:

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

See the [full api and documentation](http://package.elm-lang.org/packages/jschomay/elm-narrative-engine/latest) for more details.

I am also currently working on a visual editor that would allow non-developers to simply pick a theme and build their world and rules in a custom online text editor.

## Development blog

[Follow the development blog](http://blog.elmnarrativeengine.com) for more insights, tutorials, techniques, and information on further development of the engine and tooling.
