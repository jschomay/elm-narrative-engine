module Engine exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Components.CurrentSummary exposing (currentSummary)
import Components.Storyline exposing (storyline, Story, Storyline)
import Components.Locations exposing (locations, Location, Locations)
import Components.Inventory exposing (inventory, Item, Items)


-- APP
-- load : { locations : Locations a } -> Program Never


start initialData clientUpdate =
    Html.beginnerProgram { model = init initialData, view = view, update = engineUpdate clientUpdate }



-- MODEL


type alias Model location item story character =
    { locations : Locations location
    , inventory : Items item
    , storyline : Storyline story
    , title : String
    , current :
        { location :
            String
            -- Location location
        , characters : List (Character character)
        , items : Items item
        }
    }


type Character a
    = Character a



-- init : Locations a -> Model location item story character


init initialData =
    { locations = initialData.locations
    , inventory = initialData.inventory
    , storyline = initialData.story
    , title = ""
    , current =
        { location = ""
        , characters = []
        , items = []
        }
    }



-- UPDATE


type Msg action
    = NoOp
    | UseItem action



-- update : Msg action -> Model location item story character -> Model location item story character


engineUpdate clientUpdate msg model =
    case msg of
        NoOp ->
            model

        UseItem action ->
            { model
                | storyline = clientUpdate action model :: model.storyline
            }



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
                [ locations model.locations
                , Html.map (UseItem) <| inventory model.inventory
                ]
            ]
        ]
