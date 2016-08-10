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
    , interationsCount : Dict String Int
    , storyState : StoryState a
    }


init : String -> Scene a -> StoryState a -> Model a
init title startingScene initialState =
    { title = title
    , byline = "byline"
    , preface = "preface"
    , currentScene = startingScene
    , interationsCount = Dict.empty
    , storyState = initialState
    }


loadStory : String -> StoryElementsConfig a -> Scene a -> StoryState a -> Program Never
loadStory title storyElements startingScene initialState =
    Html.beginnerProgram
        { model = init title startingScene initialState
        , view =  view storyElements
        , update = update storyElements
        }


type Msg storyItem
    = NoOp
    | Interact storyItem



-- UPDATE


update storyElements action ({ storyState } as model) =
    let
        defaultUpdate tag =
            { model
                | storyState = { storyState | storyLine = (getDescription storyElements tag) :: model.storyState.storyLine }
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


view storyElements model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text model.title ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ Html.map (\(Components.CurrentSummary.InteractWithStage a) -> Interact a) <| currentSummary storyElements model.storyState
                , storyline model.storyState.storyLine
                ]
            , div [ class "Layout__Sidebar" ]
                [ Html.map (\(Components.Locations.InteractWithLocation a) -> Interact a) <| locations storyElements model.storyState.knownLocations
                , Html.map (\(Components.Inventory.InteractWithItem a) -> Interact a) <| inventory storyElements model.storyState.inventory
                ]
            ]
        ]
