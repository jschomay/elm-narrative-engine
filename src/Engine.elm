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


type alias Model a b =
    { title : String
    , byline : String
    , preface : String
    , interationsCount : Dict String Int
    , storyState : StoryState a b
    }


init : String -> StoryState a b -> Model a b
init title initialState =
    { title = title
    , byline = "byline"
    , preface = "preface"
    , interationsCount = Dict.empty
    , storyState = initialState
    }


loadStory : String -> StoryElementsConfig a -> StoryRulesConfig b a -> StoryState a b -> Program Never
loadStory title storyElements storyRules initialState =
    Html.beginnerProgram
        { model = init title initialState
        , view = view storyElements
        , update = update storyElements storyRules
        }


type Msg a
    = NoOp
    | Interact a



-- UPDATE


update : StoryElementsConfig a -> StoryRulesConfig b a -> Msg a -> Model a b -> Model a b
update storyElements storyRules action ({ storyState } as model) =
    let
        defaultUpdate storyElement =
            { model
                | storyState = { storyState | storyLine = (getDescription storyElements storyElement) :: model.storyState.storyLine }
            }
    in
        case action of
            NoOp ->
                model

            Interact storyElement ->
                Maybe.withDefault (defaultUpdate storyElement)
                    <| updateFromRules storyElement storyRules model.storyState
                    `Maybe.andThen` \newStoryState ->
                                        Just { model | storyState = newStoryState }



-- main layout


view : StoryElementsConfig a -> Model a b -> Html (Msg a)
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
