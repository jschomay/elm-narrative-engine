module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (join)
import Color exposing (..)
import Markdown exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


type alias Model a b c d e =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , interactions : List (StoryElement a b c)
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


init : StoryInfo -> StorySetup a b c d e -> Model a b c d e
init { title, byline, prologue } storySetup =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , interactions = [ Location storySetup.startingLocation ]
    , storyState = setUpStoryWorld storySetup
    }


setUpStoryWorld : StorySetup a b c d e -> StoryState a b c d e
setUpStoryWorld { startingScene, startingLocation, startingNarration, storyWorldSetupCommands } =
    StoryState.init startingLocation startingScene
        |> \storyState -> StoryRules.updateStoryState "Begin" storyState ( storyWorldSetupCommands, Narrate startingNarration )


loadStory : StoryInfo -> StorySetup a b c d e -> DisplayInfo a b c -> SceneSelector a b c d e -> Program Never
loadStory storyInfo storySetup displayInfo storyRules =
    Html.beginnerProgram
        { model = init storyInfo storySetup
        , view = view displayInfo
        , update = update displayInfo storyRules
        }


type Msg a b c
    = NoOp
    | StartGame
    | Interaction (StoryElement a b c)



-- UPDATE


update : DisplayInfo a b c -> SceneSelector a b c d e -> Msg a b c -> Model a b c d e -> Model a b c d e
update displayInfo storyRules action model =
    let
        defaultNarration storyElement ({ storyState } as model) =
            { model
                | storyState = { storyState | storyLine = ( toName storyElement, toDescription storyElement ) :: model.storyState.storyLine }
            }

        goToLocation location ({ storyState } as model) =
            case location of
                Location location ->
                    { model
                        | storyState = { storyState | currentLocation = location }
                    }

                _ ->
                    Debug.crash "It should be impossible for a non-location element to get here"

        updateInteractions storyElement model =
            { model
                | interactions =
                    if not <| List.member storyElement model.interactions then
                        storyElement :: model.interactions
                    else
                        model.interactions
            }

        toName storyElement =
            case storyElement of
                Item item ->
                    .name <| displayInfo.items item

                Location location ->
                    .name <| displayInfo.locations location

                Character character ->
                    .name <| displayInfo.characters character

        toDescription storyElement =
            case storyElement of
                Item item ->
                    .description <| displayInfo.items item

                Location location ->
                    .description <| displayInfo.locations location

                Character character ->
                    .description <| displayInfo.characters character

        tryUpdatingFromRules storyElement model =
            let
                scene =
                    (storyRules model.storyState.currentScene)

                beenThereDoneThat =
                    (List.member storyElement model.interactions)
            in
                updateFromRules storyElement scene model.storyState beenThereDoneThat (toName storyElement)
                    `Maybe.andThen` \newStoryState ->
                                        Just { model | storyState = newStoryState }
    in
        case action of
            NoOp ->
                model

            StartGame ->
                { model | route = GamePage }

            Interaction ((Item _) as item) ->
                model
                    |> tryUpdatingFromRules item
                    |> Maybe.withDefault (defaultNarration item model)
                    |> updateInteractions item

            Interaction ((Location _) as location) ->
                model
                    |> tryUpdatingFromRules location
                    |> Maybe.withDefault (goToLocation location model |> defaultNarration location)
                    |> updateInteractions location

            Interaction ((Character _) as character) ->
                model
                    |> tryUpdatingFromRules character
                    |> Maybe.withDefault (defaultNarration character model)
                    |> updateInteractions character



-- main layout


view : DisplayInfo a b c -> Model a b c d e -> Html (Msg a b c)
view displayInfo model =
    loadPage displayInfo model


type Route
    = TitlePage
    | GamePage


loadPage : DisplayInfo a b c -> Model a b c d e -> Html (Msg a b c)
loadPage displayInfo model =
    case model.route of
        TitlePage ->
            titelPage model

        GamePage ->
            gamePage displayInfo model


titelPage : Model a b c d e -> Html (Msg a b c)
titelPage model =
    div [ class "TitlePage" ]
        [ h1 [ class "TitlePage__Title" ] [ text model.title ]
        , h3 [ class "TitlePage__Byline" ] [ text <| "An interactive story by " ++ model.byline ]
        , toHtml [ class "TitlePage__Prologue markdown-body" ] model.prologue
        , span [ class "TitlePage__StartGame", onClick StartGame ] [ text "Play" ]
        ]


gamePage : DisplayInfo a b c -> Model a b c d e -> Html (Msg a b c)
gamePage displayInfo model =
    let
        toCssColor : Color -> String
        toCssColor =
            toRgb >> \{ red, green, blue } -> String.join "" [ "rgb(", toString red, ",", toString green, ",", toString blue, ")" ]

        cssColor =
            toCssColor <| .color <| displayInfo.locations model.storyState.currentLocation
    in
        div [ class "GamePage" ]
            [ div [ class "Layout" ]
                [ div [ class "Layout__Main" ]
                    [ h1 [ class "Current-location", style [ ( "backgroundColor", cssColor ) ] ]
                        [ text <| .name <| displayInfo.locations model.storyState.currentLocation ]
                    , Html.map
                        (\msg ->
                            case msg of
                                Components.CurrentSummary.InteractWithProp a ->
                                    Interaction (Item a)

                                Components.CurrentSummary.InteractWithCharacter a ->
                                    Interaction (Character a)
                        )
                        <| currentSummary displayInfo model.storyState (flip List.member model.interactions)
                    , storyline model.storyState.storyLine
                    ]
                , div [ class "Layout__Sidebar" ]
                    [ Html.map (\(Components.Locations.InteractWithLocation a) -> Interaction (Location a))
                        <| locations displayInfo.locations model.storyState.knownLocations model.storyState.currentLocation (flip List.member model.interactions)
                    , Html.map (\(Components.Inventory.InteractWithItem a) -> Interaction (Item a))
                        <| inventory displayInfo.items model.storyState.inventory (flip List.member model.interactions)
                    ]
                ]
            ]
