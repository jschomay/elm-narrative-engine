module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import StoryWorld exposing (..)
import Scenes exposing (..)


type alias Model a =
    { title : String
    , currentScene : Scene a
    , currentLocation : a
    , storyWorld : StoryWorld a
    , inventory : List a
    , knownLocations : List a
    , storyLine : List String
    , onStage :
        { characters : List a
        , props : List a
        }
    }


type alias InitialSetup a =
    { scene : Scene a
    , location : a
    , inventory : List a
    , knownLocations : List a
    , characters : List a
    , intro : String
    , props : List a
    }


init : String -> (a -> DisplayInformation) -> InitialSetup a -> Model a
init title display initialSetup =
    { title = title
    , storyWorld = StoryWorld display
    , currentScene = initialSetup.scene
    , currentLocation = initialSetup.location
    , inventory = initialSetup.inventory
    , knownLocations = initialSetup.knownLocations
    , storyLine = initialSetup.intro :: []
    , onStage =
        { characters = initialSetup.characters
        , props = initialSetup.props
        }
    }


loadStory : String -> (a -> DisplayInformation) -> InitialSetup a -> Program Never
loadStory title display initialSetup =
    Html.beginnerProgram
        { model = init title display initialSetup
        , view = view
        , update = update
        }


type Msg storyItem
    = NoOp
    | Interact storyItem



-- UPDATE
-- update : Msg action -> Model location item story character -> Model location item story character


update action model =
    let
        defaultUpdate tag =
            { model
                | storyLine = (getDescription model.storyWorld tag) :: model.storyLine
            }

    in
        case action of
            NoOp ->
                model

            Interact tag ->
                Maybe.withDefault (defaultUpdate tag)
                    <| updateFromRules tag model.currentScene model



-- main layout
-- view : Model location item story character -> Html (Msg a)


view model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text model.title ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ Html.map (\(Components.CurrentSummary.InteractWithStage a) -> Interact a) <| currentSummary model.storyWorld model.currentLocation model.onStage.characters model.onStage.props
                , storyline model.storyLine
                ]
            , div [ class "Layout__Sidebar" ]
                [ Html.map (\(Components.Locations.InteractWithLocation a) -> Interact a) <| locations model.storyWorld model.knownLocations
                , Html.map (\(Components.Inventory.InteractWithItem a) -> Interact a) <| inventory model.storyWorld model.inventory
                ]
            ]
        ]
