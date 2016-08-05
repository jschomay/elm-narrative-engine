module StoryWorld exposing (..)

import Dict exposing (..)
import String


type alias StoryWorld =
    Dict String DisplayInformation


type DisplayInformation
    = DisplayInformation Name Description


type StoryElement a
    = StoryElement a Name Description


type alias Name =
    String


type alias Description =
    String


getDisplayInformation : StoryWorld -> a -> DisplayInformation
getDisplayInformation storyWorld tag =
    let
        notFound tag =
            "Error: \""
                ++ (toString tag)
                ++ "\" not defined in story elements"
                |> String.toUpper
    in
        Maybe.withDefault (DisplayInformation (notFound tag) (notFound tag))
            <| Dict.get (toString tag) storyWorld

getName : StoryWorld -> a -> String
getName storyWorld tag =
  let
      (DisplayInformation name description) =
        getDisplayInformation storyWorld tag
  in
      name

getDescription : StoryWorld -> a -> String
getDescription storyWorld tag =
  let
      (DisplayInformation name description) =
        getDisplayInformation storyWorld tag
  in
      description


item : a -> Name -> Description -> StoryElement a
item tag name description =
    StoryElement tag name description


buildWorld : List (StoryElement a) -> StoryWorld
buildWorld =
    List.foldl
        (\(StoryElement tag name description) acc ->
            Dict.insert (toString tag) (DisplayInformation name description) acc
        )
        Dict.empty
