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


type alias Model a b c d =
    { title : String
    , byline : String
    , preface : String
    , interactions : List (StoryElement a b c)
    , storyState : StoryState a b c d
    }


init : String -> StoryState a b c d -> Model a b c d
init title initialState =
    { title = title
    , byline = "byline"
    , preface = "preface"
    , interactions = []
    , storyState = initialState
    }


loadStory : String -> ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> SceneSelector a b c d -> StoryState a b c d -> Program Never
loadStory title itemsInfo locationsInfo charactersInfo storyRules initialState =
    Html.beginnerProgram
        { model = init title initialState
        , view = view itemsInfo locationsInfo charactersInfo
        , update = update itemsInfo locationsInfo charactersInfo storyRules
        }


type Msg a b c
    = NoOp
    | Interaction (StoryElement a b c)



-- UPDATE


update : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> SceneSelector a b c d -> Msg a b c -> Model a b c d -> Model a b c d
update itemsInfo locationsInfo charactersInfo storyRules action model =
    let
        defaultNarration : String -> String -> Model a b c d -> Model a b c d
        defaultNarration name description ({ storyState } as model) =
            { model
                | storyState = { storyState | storyLine = ( name, description ) :: model.storyState.storyLine }
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

        storyElementName storyElement =
            case storyElement of
                Item item ->
                    getName <| itemsInfo item

                Location location ->
                    getName <| locationsInfo location

                Character character ->
                    getName <| charactersInfo character

        storyElementDescription storyElement =
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
                updateFromRules storyElement scene model.storyState beenThereDoneThat (storyElementName storyElement)
                    `Maybe.andThen` \newStoryState ->
                                        Just { model | storyState = newStoryState }
    in
        case action of
            NoOp ->
                model

            Interaction ((Item _) as item) ->
                model
                    |> tryUpdatingFromRules item
                    |> Maybe.withDefault (defaultNarration (storyElementName item) (storyElementDescription item) model)
                    |> updateInteractions item

            Interaction (Location location) ->
                model
                    |> tryUpdatingFromRules (Location location)
                    |> Maybe.withDefault (goToLocation location model)
                    |> updateInteractions (Location location)

            Interaction ((Character _) as character) ->
                model
                    |> tryUpdatingFromRules character
                    |> Maybe.withDefault (defaultNarration (storyElementName character) (storyElementDescription character) model)
                    |> updateInteractions character



-- main layout


view : ItemsInfo a -> LocationsInfo b -> CharactersInfo c -> Model a b c d -> Html (Msg a b c)
view itemsInfo locationsInfo charactersInfo model =
    let
        cssColor =
            toCssColor <| getColor <| locationsInfo model.storyState.currentLocation
    in
        div [ class "Page" ]
            [ h1 [ class "Title", style [ ( "backgroundColor", cssColor ) ] ]
                [ text <| getName <| locationsInfo model.storyState.currentLocation ]
            , div [ class "Layout" ]
                [ div [ class "Layout__Main" ]
                    [ Html.map
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
