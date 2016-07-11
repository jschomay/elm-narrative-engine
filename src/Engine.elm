module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Components.CurrentSummary exposing (..)
import Components.Storyline exposing (..)
import Components.Locations exposing (..)
import Components.Inventory exposing (..)


-- APP
-- load : { locations : Locations a } -> Program Never


start narrativeContent storyProgressionLogic storyStartingPoint =
    Html.beginnerProgram
        { model = init narrativeContent storyStartingPoint
        , view = view
        , update = update storyProgressionLogic
        }



-- MODEL
-- StoryElement details:
-- Item, Location, Character
-- StoryElement clientTag displayName text


type Subject
    = Item
    | Location
    | Character


type alias Scenario =
    List Condition


type Condition a
    = Matcher (Subject a)


type StoryRule
    = StoryRule Subject Scenario (List StoryCommand)


type Scene
    = List StoryRule


type alias NarrativeContent =
    List Scene



--
-- type StoryElement a
--     = StoryElement a String String
-- type alias NarrativeContent a =
--     List (StoryElement a)


type Msg storyItem
    = NoOp
    | Interact storyItem


type CycleType
    = Randomly
    | InOrder


type ClientCommand subject
    = Narrate String
    | NarrateOneOf (List String) CycleType
    | AddToInventory subject
    | RemoveFromInventory subject
    | None


type alias Model storyElement location item character =
    { narrativeContent : NarrativeContent storyElement
    , knownLocations : KnownLocations location
    , inventory : Items item
    , storyline : Storyline
    , title : String
    , current :
        { location :
            String
            -- Location location
        , characters : List (Character character)
        , items : Items item
        }
    , narrativeContent : NarrativeContent storyElement
    }



-- init : Locations a -> Model location item story character


init narrativeContent storyStartingPoint =
    { narrativeContent = narrativeContent
    , knownLocations = storyStartingPoint.knownLocations
    , inventory = storyStartingPoint.inventory
    , storyline = storyStartingPoint.story
    , title = ""
    , current =
        { location = ""
        , characters = []
        , items = []
        }
    }



-- UPDATE
-- update : Msg action -> Model location item story character -> Model location item story character


update storyProgressionLogic action model =
    case action of
        NoOp ->
            model

        Interact storyItem ->
            updateModelFromClientCommands model <| storyProgressionLogic storyItem



-- updateModelFromClientCommands : Model -> List StoryUpdateCommands -> Model
-- todo, make cmd a batched list to map over


updateModelFromClientCommands model cmd =
    let
        getStoryElement subject =
            List.head
                <| List.filter
                    (\(StoryElement tag name text) ->
                        tag == subject
                    )
                    model.narrativeContent
    in
        case cmd of
            ContinueStory subject ->
                { model
                    | storyline = addToStoryline model.storyline (getStoryElement subject)
                }

            None ->
                model

            _ ->
                model


addToStoryline storyline storyElement =
    case storyElement of
        Just (StoryElement tag name text) ->
            Components.Storyline.update storyline text

        Nothing ->
            storyline



-- main layout
-- view : Model location item story character -> Html (Msg a)


view model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text "The Peculiar Adventures of Pinkleton Short" ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ currentSummary model
                , storyline model.storyline
                ]
            , div [ class "Layout__Sidebar" ]
                [ locations model.knownLocations
                , Html.map (\(InteractWithItem storyItem) -> Interact storyItem) <| inventory model.inventory
                ]
            ]
        ]
