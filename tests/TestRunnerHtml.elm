module TestRunnerHtml exposing (..)

import Test exposing (concat)
import Test.Runner.Html
import Tests exposing (suite)


main : Program Never
main =
    [ suite ]
        |> concat
        |> Test.Runner.Html.run
