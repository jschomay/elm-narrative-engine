module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Dict exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


type alias Model a =
    { title : String
    , byline : String
    , preface : String
    , currentScene : Scene a
    , storyElements : StoryElements
    , interationsCount : Dict String Int
    , storyState : StoryState a
    }


init : String -> Scene a -> List (StoryElement a) -> StoryState a -> Model a
init title startingScene storyElements initialState =
    { title = title
    , byline = "byline"
    , preface = "preface"
    , currentScene = startingScene
    , storyElements = buildStoryElements storyElements
    , interationsCount = Dict.empty
    , storyState = initialState
    }


loadStory : String -> Scene a -> List (StoryElement a) -> StoryState a -> Program Never
loadStory title startingScene storyElements initialState =
    Html.beginnerProgram
        { model = init title startingScene storyElements initialState
        , view = view
        , update = update
        }


type Msg storyItem
    = NoOp
    | Interact storyItem



-- UPDATE
-- update : Msg action -> Model location item story character -> Model location item story character


update action ({ storyState } as model) =
    let
        defaultUpdate tag =
            { model
                | storyState = { storyState | storyLine = (getDescription model.storyElements tag) :: model.storyState.storyLine }
            }
    in
        case action of
            NoOp ->
                model

            Interact tag ->
                Maybe.withDefault (defaultUpdate tag)
                    <| updateFromRules tag model.currentScene model.storyState
                    `Maybe.andThen` \newStoryState ->
                                        Just { model | storyState = newStoryState }



-- main layout
-- view : Model location item story character -> Html (Msg a)


view model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text model.title ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ Html.map (\(Components.CurrentSummary.InteractWithStage a) -> Interact a) <| currentSummary model.storyElements model.storyState
                , storyline model.storyState.storyLine
                ]
            , div [ class "Layout__Sidebar" ]
                [ Html.map (\(Components.Locations.InteractWithLocation a) -> Interact a) <| locations model.storyElements model.storyState.knownLocations
                , Html.map (\(Components.Inventory.InteractWithItem a) -> Interact a) <| inventory model.storyElements model.storyState.inventory
                ]
            ]
        ]
