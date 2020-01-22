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
            """
            ON: *.location.dark
            """
            "It's too dark to go in there!"
        , rule "entering the cave with a light source"
            """
            ON: CAVE
            IF: *.item.illumination>5.current_location=PLAYER
            DO: PLAYER.current_location=CAVE.fear+2
            """
            "You enter the cave, {PLAYER.fear>4? your heart pounding | bravely}..."

        -- etc...
        ]

```

See https://github.com/jschomay/elm-interactive-story-starter for a full example.

## Features

- **"Schemaless" world model** - define your world any way you like
- **"Salience-based" and "quality-based" rule matching system** - craft immersive and reactive stories
- **"View agnostic"** - build your views however you want

Additional features:

- Import all data from external sources (like a spreadsheet)
- Simple and intuitive authoring syntax
- Advanced querying system
- Embeddable in a larger code base 
- Designed with the Entity-Component-System pattern


## Usage

The engine has two main parts: the world model, and the rules.  You can use the 
authoring syntax shown above to define both of these.

From there, you can query the world model as needed to build your view, and you can find the best matching rule when a player interacts with an entity given the current state of the world, to update the world and show side effects like story text.

See the example link above for more context.


## More information

Read the [dev blog](http://blog.elmnarrativeengine.com)

[Play sample stories made with the Elm Narrative Engine](http://blog.elmnarrativeengine.com/sample-stories/)
