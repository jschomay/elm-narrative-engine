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


load initialData =
    Html.beginnerProgram { model = init initialData, view = view, update = update }



-- MODEL


type alias Model location item story character =
    { locations : Locations location
    , inventory : Items item
    , storyline : Storyline story
    , title : String
    , current :
        { locationName : String
        , locationDescription : String
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
        { locationName = ""
        , locationDescription = ""
        , characters = []
        , items = []
        }
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model location item story character -> Model location item story character
update msg model =
    case msg of
        NoOp ->
            model



-- main layout


view : Model location item story character -> Html Msg
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
                , inventory model.inventory
                ]
            ]
        ]
