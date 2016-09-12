module Story exposing (load, Info, Setup)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (..)
import Story.Element exposing (..)
import Story.Rule exposing (..)
import Story.State exposing (..)
import Story.Mechanics exposing (..)
import Views.Game exposing (..)


type alias Model a b c d e =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , storyState : StoryState a b c d e
    }


type alias Setup a b c d e =
    { startingScene : d
    , startingLocation : b
    , startingNarration : String
    , setupCommands : ChangeWorldCommands a b c d e
    }


type alias Info =
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
    | Interaction (Story.Mechanics.Msg a b c)


init : Info -> Setup a b c d e -> Model a b c d e
init { title, byline, prologue } setup =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , storyState = setUpWorld setup
    }


setUpWorld : Setup a b c d e -> StoryState a b c d e
setUpWorld { startingScene, startingLocation, startingNarration, setupCommands } =
    Story.State.init startingLocation startingScene
        |> \storyState -> Story.State.advanceStory "Begin" storyState ( setupCommands, Narrate startingNarration )


load : Info -> Elements a b c -> Setup a b c d e -> (d -> Scene a b c d e) -> Program Never
load info elements setup scenes =
    Html.beginnerProgram
        { model = init info setup
        , view = view elements
        , update = update elements scenes
        }



-- UPDATE


update : Elements a b c -> (d -> Scene a b c d e) -> Msg a b c -> Model a b c d e -> Model a b c d e
update displayInfo scenes msg model =
    case msg of
        NoOp ->
            model

        StartGame ->
            { model | route = GamePage }

        Interaction msg ->
            { model | storyState = Story.Mechanics.update displayInfo scenes msg model.storyState }



-- VIEW


view : Elements a b c -> Model a b c d e -> Html (Msg a b c)
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
