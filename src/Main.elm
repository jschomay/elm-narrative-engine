module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Components.CurrentSummary exposing (currentSummary)
import Components.Storyline exposing (storyline)
import Components.Locations exposing (locations)
import Components.Inventory exposing (inventory)


-- APP


main : Program Never
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    String


model : String
model =
    "Elm Narrative Engine"



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- main layout


view : Model -> Html Msg
view model =
    div [ class "Page" ]
        [ h1 [ class "Title" ]
            [ text "The Peculiar Adventures of Pinkleton Short" ]
        , div [ class "Layout" ]
            [ div [ class "Layout__Main" ]
                [ currentSummary model
                , storyline model
                ]
            , div [ class "Layout__Sidebar" ]
                [ locations model
                , inventory model
                ]
            ]
        ]
