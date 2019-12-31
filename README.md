# Elm Narrative Engine

A tool for building interactive story games.

### Example: 

```elm
worldModel =
    parseWorldModel
        [ entity "PLAYER.fear=1"
        , entity "TORCH.item.illumination=7.current_location=PLAYER"
        , entity "CAVE.location.dark"

        -- etc...
        ]


rules =
    parseRules
        [ rule "entering dark places"
            { trigger = "*.place.dark"
            , conditions = []
            , changes = []
            , narrative = "It's too dark to go in there!"
            }
        , rule "entering the cave with a light source"
            { trigger = "CAVE"
            , conditions = [ "*.item.illumination>5.current_location=PLAYER" ]
            , changes = [ "PLAYER.current_location=CAVE.fear+2" ]
            , narrative = "You enter the cave, {PLAYER.fear>4? your heart pounding | bravely}..."
            }

        -- etc...
        ]

```

## Features

### Flexible

- Define your world model any way you like
- Build your views however you need
- World model, rules, and narrative content can be imported from external sources

### Powerful

- Designed with "salience-based" and "quality-based" narrative systems for immersive and responsive stories
- Advanced querying system
- Data-driven and declarative
- Simple and intuitive authoring syntax

### Extendable

- Use as a "narrative system" component alongside other game code
- Use the Entity Component System pattern (via Extensible Records)


## Usage

See the [full working example](https://github.com/jschomay/elm-narrative-engine/blob/master/src/Example.elm).

The engine has two main parts: the world model, and the rules.  You can use the authoring syntax shown above to define both of these.

From there, you can query the world model as needed to build your view, and you can find the best matching rule when a player interacts with an entity given the current state of the world, to update the world and show side effects like story text.


## More information

Read the [dev blog](http://blog.elmnarrativeengine.com)

[Play sample stories made with the Elm Narrative Engine](http://blog.elmnarrativeengine.com/sample-stories/)
