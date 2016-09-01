module Components.Storyline exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Markdown


storyline : List ( String, String ) -> Html msg
storyline storyLine =
    let
        storyLi i ( storyElementName, storyText ) =
            let
                numLines =
                    List.length storyLine

                key =
                    storyElementName ++ (toString <| numLines - i)

                classes =
                    [ ( "Storyline__Item", True )
                    , ( "u-fade-in", i == 0 )
                    ]
            in
                ( key
                , li [ classList classes ]
                    [ h4 [ class "Storyline__Item__Action" ] <| [ text storyElementName ]
                    , Markdown.toHtml [ class "Storyline__Item__Narration markdown-body" ] storyText
                    ]
                )
    in
        Html.Keyed.ol [ class "Storyline" ]
            (List.indexedMap storyLi storyLine)
