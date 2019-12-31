module NarrativeEngine.Debug exposing (State, debugBar, init, setLastInteractionId, setLastMatchedRuleId, updateSearch)

{-| A helper module to display a useful debugging tool during development.

@docs State, debugBar, init, setLastInteractionId, setLastMatchedRuleId, updateSearch

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import NarrativeEngine.Core.WorldModel exposing (WorldModel)
import Set


{-| The state needed to render the debug bar. Track this in your Model.
-}
type State
    = State
        { searchText : String
        , lastMatchedRuleId : String
        , lastInteractionId : String
        }


{-| An empty debug state to start off with.
-}
init : State
init =
    State
        { searchText = ""
        , lastMatchedRuleId = "Begin"
        , lastInteractionId = "Start"
        }


{-| Call this any time the debug bar's search field changes (ie. the update code for the `msg` supplied to `debugBar`.
-}
updateSearch : String -> State -> State
updateSearch text (State state) =
    State { state | searchText = text }


{-| Set the last matched rule id (call this upon each interaction, use the interaction id if no rules matched).
-}
setLastMatchedRuleId : String -> State -> State
setLastMatchedRuleId id (State state) =
    State { state | lastMatchedRuleId = id }


{-| Set the last interaction id (call this upon each interaction).
-}
setLastInteractionId : String -> State -> State
setLastInteractionId id (State state) =
    State { state | lastInteractionId = id }


{-| A minimal "debug bar" that you can place at the top or bottom of your screen. Shows the last matched rule id and last interaction id. Includes a search field to search/filter the current world model's state. Entities are displayed in the "entity syntax" described in `Utils.EntityParser`. This makes it easy to search for an entity by ID. You can also search for a tag to show all entities with that tag. You can also find all linked entities in a similar fashion. "." will show all entities. Clear the search field to hide matching entities.

You must include the message that you use to call `updateSearch`, which will get triggered on each changed input. Also include the current world model, and current debug state.

Note, if your client responds to key events, you may need to stop propagation on those events from the debug bar. You can do something like this:

    div [ stopPropagationOn "keydown" <| Json.succeed ( NoOp, True ) ]
        [ Debug.debugBar DebugSeachWorldModelMsg model.worldModel model.debugState ]

-}
debugBar : (String -> msg) -> WorldModel a -> State -> Html msg
debugBar msg worldModel (State { lastInteractionId, lastMatchedRuleId, searchText }) =
    let
        displayWorldModel =
            worldModel
                |> Dict.toList
                |> List.map displayEntity

        displayEntity ( id, { tags, stats, links } ) =
            String.join "." <|
                List.filter (not << String.isEmpty) <|
                    List.map (String.join ".")
                        [ [ id ]
                        , Set.toList tags
                        , Dict.toList stats |> List.map (\( key, value ) -> String.join "=" [ key, String.fromInt value ])
                        , Dict.toList links |> List.map (\( key, value ) -> String.join "=" [ key, value ])
                        ]

        filteredDisplayWorldModel =
            if String.isEmpty searchText then
                []

            else
                List.filter (fuzzyMatch searchText) displayWorldModel
                    |> List.sortBy
                        (\text ->
                            if String.startsWith (String.toLower searchText) (String.toLower text) then
                                -1

                            else
                                0
                        )

        fuzzyMatch search text =
            String.contains (String.toLower search) (String.toLower text)
    in
    div
        [ style "color" "yellow"
        , style "background" "black"
        , style "opacity" "0.9"
        , style "lineHeight" "1.5em"
        , style "zIndex" "99"
        , style "position" "absolute"
        ]
        [ text "Debug mode"
        , input
            [ onInput msg
            , value searchText
            , placeholder "Search world model"
            , style "margin" "0 10px"
            ]
            []
        , span [] [ text <| "Last triggered rule: " ++ lastInteractionId ++ " - " ++ lastMatchedRuleId ]
        , ul [ style "borderTop" "1px solid #333" ] <| List.map (\e -> li [] [ text e ]) filteredDisplayWorldModel
        ]
