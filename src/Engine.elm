module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


type alias Model a b =
    { title : String
    , byline : String
    , preface : String
    , interactions : List a
    , storyState : StoryState a b
    }


init : String -> StoryState a b -> Model a b
init title initialState =
    { title = title
    , byline = "byline"
    , preface = "preface"
    , interactions = []
    , storyState = initialState
    }


loadStory : String -> StoryElementsConfig a -> StoryRulesConfig a b -> StoryState a b -> Program Never
loadStory title storyElements storyRules initialState =
    Html.beginnerProgram
        { model = init title initialState
        , view = view storyElements
        , update = update storyElements storyRules
        }


type Msg a
    = NoOp
    | Interact a
    | InteractWithLocation a



-- UPDATE


update : StoryElementsConfig a -> StoryRulesConfig a b -> Msg a -> Model a b -> Model a b
update storyElements storyRules action model =
    let
        narrateTheDescription storyElement ({ storyState } as model) =
            { model
                | storyState = { storyState | storyLine = ( storyElement, getDescription storyElements storyElement ) :: model.storyState.storyLine }
            }

        goToLocation location ({ storyState } as model) =
            { model
                | storyState = { storyState | currentLocation = location }
            }

        updateInteractions storyElement model =
            { model
                | interactions =
                    if not <| List.member storyElement model.interactions then
                        storyElement :: model.interactions
                    else
                        model.interactions
            }

        tryUpdatingFromRules storyElement model =
            updateFromRules storyElement storyRules model.storyState (flip List.member model.interactions)
                `Maybe.andThen` \newStoryState ->
                                    Just { model | storyState = newStoryState }
    in
        case action of
            NoOp ->
                model

            InteractWithLocation location ->
                model
                    |> tryUpdatingFromRules location
                    |> Maybe.withDefault (goToLocation location model)
                    |> updateInteractions location

            Interact storyElement ->
                model
                    |> tryUpdatingFromRules storyElement
                    |> Maybe.withDefault (narrateTheDescription storyElement model)
                    |> updateInteractions storyElement



-- main layout


view : StoryElementsConfig a -> Model a b -> Html (Msg a)
view storyElements model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text <| getName storyElements model.storyState.currentLocation ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ Html.map (\(Components.CurrentSummary.InteractWithStage a) -> Interact a)
                    <| currentSummary storyElements model.storyState (flip List.member model.interactions)
                , storyline storyElements model.storyState.storyLine
                ]
            , div [ class "Layout__Sidebar" ]
                [ Html.map (\(Components.Locations.InteractWithLocation a) -> InteractWithLocation a)
                    <| locations storyElements model.storyState.knownLocations model.storyState.currentLocation (flip List.member model.interactions)
                , Html.map (\(Components.Inventory.InteractWithItem a) -> Interact a)
                    <| inventory storyElements model.storyState.inventory (flip List.member model.interactions)
                ]
            ]
        ]
