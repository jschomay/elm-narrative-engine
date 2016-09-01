module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


type alias Model a b c d =
    { title : String
    , byline : String
    , prologue : String
    , route : Route
    , interactions : List (StoryElement a b c)
    , storyState : StoryState a b c d
    }


init : String -> String -> String -> StoryState a b c d -> Model a b c d
init title byline prologue initialState =
    { title = title
    , byline = byline
    , prologue = prologue
    , route = TitlePage
    , interactions = []
    , storyState = initialState
    }


loadStory : String -> String -> String -> ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> SceneSelector a b c d -> StoryState a b c d -> Program Never
loadStory title byline prologue itemsInfo locationsInfo charactersInfo storyRules initialState =
    Html.beginnerProgram
        { model = init title byline prologue initialState
        , view = view itemsInfo locationsInfo charactersInfo
        , update = update itemsInfo locationsInfo charactersInfo storyRules
        }


type Msg a b c
    = NoOp
    | StartGame
    | Interaction (StoryElement a b c)



-- UPDATE


update : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> SceneSelector a b c d -> Msg a b c -> Model a b c d -> Model a b c d
update itemsInfo locationsInfo charactersInfo storyRules action model =
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
                    getName <| itemsInfo item

                Location location ->
                    getName <| locationsInfo location

                Character character ->
                    getName <| charactersInfo character

        toDescription storyElement =
            case storyElement of
                Item item ->
                    getDescription <| itemsInfo item

                Location location ->
                    getDescription <| locationsInfo location

                Character character ->
                    getDescription <| charactersInfo character

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


view : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> Model a b c d -> Html (Msg a b c)
view itemsInfo locationsInfo charactersInfo model =
    loadPage itemsInfo locationsInfo charactersInfo model


type Route
    = TitlePage
    | GamePage


loadPage : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> Model a b c d -> Html (Msg a b c)
loadPage itemsInfo locationsInfo charactersInfo model =
    case model.route of
        TitlePage ->
            titelPage model

        GamePage ->
            gamePage itemsInfo locationsInfo charactersInfo model


titelPage : Model a b c d -> Html (Msg a b c)
titelPage model =
    div [ class "TitlePage" ]
        [ h1 [ class "TitlePage__Title" ] [ text model.title ]
        , h3 [ class "TitlePage__ByLine" ] [ text <| "An interactive story by " ++ model.byline ]
        , toHtml [ class "TitlePage__Prologue markdown-body" ] model.prologue
        , span [ class "TitlePage__StartGame", onClick StartGame ] [ text "Play" ]
        ]


gamePage : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> Model a b c d -> Html (Msg a b c)
gamePage itemsInfo locationsInfo charactersInfo model =
    let
        cssColor =
            toCssColor <| getColor <| locationsInfo model.storyState.currentLocation
    in
        div [ class "GamePage" ]
            [ div [ class "Layout" ]
                [ div [ class "Layout__Main" ]
                    [ h1 [ class "Current-location", style [ ( "backgroundColor", cssColor ) ] ]
                        [ text <| getName <| locationsInfo model.storyState.currentLocation ]
                    , Html.map
                        (\msg ->
                            case msg of
                                Components.CurrentSummary.InteractWithProp a ->
                                    Interaction (Item a)

                                Components.CurrentSummary.InteractWithCharacter a ->
                                    Interaction (Character a)
                        )
                        <| currentSummary itemsInfo locationsInfo charactersInfo model.storyState (flip List.member model.interactions)
                    , storyline model.storyState.storyLine
                    ]
                , div [ class "Layout__Sidebar" ]
                    [ Html.map (\(Components.Locations.InteractWithLocation a) -> Interaction (Location a))
                        <| locations locationsInfo model.storyState.knownLocations model.storyState.currentLocation (flip List.member model.interactions)
                    , Html.map (\(Components.Inventory.InteractWithItem a) -> Interaction (Item a))
                        <| inventory itemsInfo model.storyState.inventory (flip List.member model.interactions)
                    ]
                ]
            ]
