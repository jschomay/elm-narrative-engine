# Elm Narrative Engine

A tool for building interactive story games.

## Features

### Flexible

- No expectations or restrictions on how you build your world model
- View-agnostic

### Powerful

- Designed with "salience-based" and "quality-based" narrative systems for immersive and responsive stories
- Advanced querying system

### Extendable

- Designed to be used as a "narrative system" component alongside other game code
- Designed with extensible records, making it easy to use the Entity Component System pattern
- Data-driven and declarative - story data can be imported


## Usage

See the [full working example](https://github.com/jschomay/elm-narrative-engine/blob/master/src/Example.elm).

The engine has two main parts: the world model, and the rules.

**Define your "World model"**

```elm
worldModel =
    Dict.fromList
        [ entity "PLAYER"
            |> stat "fear" 1
        , entity "TORCH"
            |> tag "item"
            |> stat "illumination" 7
            |> link "location" "PLAYER"
        , entity "CAVE"
            |> tag "place"
            |> tag "dark"
        , etc...
        ]
```

**Define your rules**

```elm
rules =
    Dict.fromList
        [ rule "too dark to enter"
            { trigger = MatchAny [ HasTag "place", HasTag "dark" ]
            , conditions = []
            , changes = []
            , narrative = "It's too dark to go in there!"
            }
          )
        , rule "entering the cave with torch"
            { trigger = Match "CAVE" []
            , conditions = [ Match "TORCH" [ HasLink "location" ( Match "PLAYER" [] ) ] ]
            , changes = [ Update "PLAYER" [ SetLink "location" "CAVE", IncStat "fear" 2 ] ]
            , narrative = "You enter the cave, your heart pounding..."
            }
          )
        , etc...
        ]
```

From there, you can query the world model to build your view, and you can find the best matching rule when a player interacts with an entity given the current state of the world, to update the world and show side effects like story text.


## More information

Read the [dev blog](http://blog.elmnarrativeengine.com)

[Play sample stories made with the Elm Narrative Engine](http://blog.elmnarrativeengine.com/sample-stories/)
