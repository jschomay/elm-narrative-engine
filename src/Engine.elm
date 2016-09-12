module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)
import Mechanics exposing (..)
import Views.Game exposing (..)


type alias Model a b c d e =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , storyState : StoryState a b c d e
    }


type alias StorySetup a b c d e =
    { startingScene : d
    , startingLocation : b
    , startingNarration : String
    , storyWorldSetupCommands : ChangeWorldCommands a b c d e
    }


type alias StoryInfo =
    { title : String
    , byline : String
    , prologue : String
    }


type Route
    = TitlePage
    | GamePage


type Msg a b c
    = NoOp
    | StartGame
    | Interaction (Mechanics.Msg a b c)


init : StoryInfo -> StorySetup a b c d e -> Model a b c d e
init { title, byline, prologue } storySetup =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , storyState = setUpStoryWorld storySetup
    }


setUpStoryWorld : StorySetup a b c d e -> StoryState a b c d e
setUpStoryWorld { startingScene, startingLocation, startingNarration, storyWorldSetupCommands } =
    StoryState.init startingLocation startingScene
        |> \storyState -> StoryState.advanceStory "Begin" storyState ( storyWorldSetupCommands, Narrate startingNarration )


loadStory : StoryInfo -> StorySetup a b c d e -> DisplayInfo a b c -> (d -> Scene a b c d e) -> Program Never
loadStory storyInfo storySetup displayInfo scenes =
    Html.beginnerProgram
        { model = init storyInfo storySetup
        , view = view displayInfo
        , update = update displayInfo scenes
        }



-- UPDATE


update : DisplayInfo a b c -> (d -> Scene a b c d e) -> Msg a b c -> Model a b c d e -> Model a b c d e
update displayInfo scenes msg model =
    case msg of
        NoOp ->
            model

        StartGame ->
            { model | route = GamePage }

        Interaction msg ->
            { model | storyState = Mechanics.update displayInfo scenes msg model.storyState }



-- VIEW


view : DisplayInfo a b c -> Model a b c d e -> Html (Msg a b c)
view displayInfo model =
    case model.route of
        TitlePage ->
            titelPage model

        GamePage ->
            Html.map Interaction <| Views.Game.view displayInfo model.storyState


titelPage : Model a b c d e -> Html (Msg a b c)
titelPage model =
    div [ class "TitlePage" ]
        [ h1 [ class "TitlePage__Title" ] [ text model.title ]
        , h3 [ class "TitlePage__Byline" ] [ text <| "An interactive story by " ++ model.byline ]
        , toHtml [ class "TitlePage__Prologue markdown-body" ] model.prologue
        , span [ class "TitlePage__StartGame", onClick StartGame ] [ text "Play" ]
        ]
