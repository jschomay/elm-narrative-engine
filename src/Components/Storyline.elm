module Components.Storyline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import StoryElements exposing (..)


storyline : StoryElementsConfig a -> List ( a, String ) -> Html msg
storyline storyElements storyLine =
    let
        storyItem ( storyElement, storyText ) =
            li [ class "Storyline__Item" ]
                [ h4 [ class "Storyline__Item__Action" ] <| [ text <| getName storyElements storyElement ]
                , Markdown.toHtml [ class "Storyline__Item__Narration" ] storyText
                ]
    in
        ul [ class "Storyline" ]
            (List.map storyItem storyLine)
