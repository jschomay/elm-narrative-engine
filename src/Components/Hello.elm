module Components.Hello exposing (..)

import Html exposing (..)


hello : String -> Html a
hello model =
    h1 []
        [ text ("Hello, " ++ model) ]
