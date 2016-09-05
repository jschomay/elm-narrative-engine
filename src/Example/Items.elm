module Items exposing (..)

import StoryElements exposing (..)

type MyItem
    = Envelope
    | Podium
    | Mic
    | KitchenExit



storyItems : ItemsInfo MyItem
storyItems tag =
    case tag of
        KitchenExit ->
            item "emergency exit" "The handle has a warning: \"Opening will sound the alarm\""

        Envelope ->
            item "thick envelope" "You found it crammed in your pocket, but you don't recoginze it."

        Podium ->
            item "podium" "The podium is on the stage, facing the audience.  Right in the spotlight."

        Mic ->
            item "microphone" "\"Testing one, two.\" Yup, it works fine."


