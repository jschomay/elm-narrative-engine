module Views.Storyline exposing (..)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Types exposing (..)


storyline : List ( String, String ) -> Html (Msg item loation character)
storyline storyLine =
    let
        storyLi i ( interactableName, storyText ) =
            let
                numLines =
                    List.length storyLine

                key =
                    interactableName ++ (toString <| numLines - i)

                classes =
                    [ ( "Storyline__Item", True )
                    , ( "u-fade-in", i == 0 )
                    ]
            in
                ( key
                , li [ classList classes ]
                    <| [ h4 [ class "Storyline__Item__Action" ] <| [ text interactableName ]
                       , Markdown.toHtml [ class "Storyline__Item__Narration markdown-body" ] storyText
                       ]
                    ++ if i == 0 then
                        []
                       else
                        [ span
                            [ class "Storyline__Item__Rollback"
                            , onClick <| Rollback (numLines - i - 1)
                            ]
                            []
                        ]
                )
    in
        Html.Keyed.ol [ class "Storyline" ]
            (List.indexedMap storyLi storyLine)
