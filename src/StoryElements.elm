module StoryElements exposing (..)

import Dict exposing (..)
import String


type alias StoryElements =
    Dict String Details


type Details
    = Details Name Description


type StoryElement a
    = StoryElement a Name Description


type alias Name =
    String


type alias Description =
    String


getDetails : StoryElements -> a -> Details
getDetails storyElements tag =
    let
        notFound tag =
            "Error: \""
                ++ (toString tag)
                ++ "\" not defined in story elements"
                |> String.toUpper
    in
        Maybe.withDefault (Details (notFound tag) (notFound tag))
            <| Dict.get (toString tag) storyElements


getName : StoryElements -> a -> String
getName storyElements tag =
    let
        (Details name description) =
            getDetails storyElements tag
    in
        name


getDescription : StoryElements -> a -> String
getDescription storyElements tag =
    let
        (Details name description) =
            getDetails storyElements tag
    in
        description


buildStoryElements : List (StoryElement a) -> StoryElements
buildStoryElements =
    List.foldl
        (\(StoryElement tag name description) acc ->
            Dict.insert (toString tag) (Details name description) acc
        )
        Dict.empty
