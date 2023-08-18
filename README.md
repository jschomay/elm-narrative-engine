# Elm Narrative Engine

A tool for building non-linear interactive story games.

## Usage


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


inventoryView =
    query "*.item.current_location=PLAYER" worldModel |> List.map item_view
```


Clone https://github.com/jschomay/elm-interactive-story-starter to get started.

See the [visual editor](https://enegames.itch.io/elm-narrative-engine) to easily author you content and import it into your custom game.
