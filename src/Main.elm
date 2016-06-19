module Main exposing (..)

import Html exposing (..)


-- import Html.Attributes exposing (..)

import Html.App as Html


-- import Html.Events exposing (onClick)

import Components.Hello exposing (hello)


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


view : Model -> Html Msg
view model =
    div []
        [ hello model
        ]
