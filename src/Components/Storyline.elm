module Components.Storyline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

storyline : List String -> Html a
storyline storyLine =
    let
        storyItem storyText =
            li [ class "Storyline__Prose" ]
                [ Markdown.toHtml [] storyText ]
    in
        ul [ class "Storyline" ]
            (List.map storyItem storyLine)
