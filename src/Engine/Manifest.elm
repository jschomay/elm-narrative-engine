module Engine.Manifest exposing
    ( character
    , characterIsInLocation
    , getCharactersInLocation
    , getItemsInInventory
    , getItemsInLocation
    , getLocations
    , init
    , isCharacter
    , isItem
    , isLocation
    , item
    , itemIsInInventory
    , itemIsInLocation
    , location
    , update
    )

import Dict exposing (Dict)
import Types exposing (..)


init :
    { items : List String
    , locations : List String
    , characters : List String
    }
    -> Manifest
init { items, locations, characters } =
    let
        insertFn interactableConstructor id acc =
            Dict.insert id interactableConstructor acc

        foldFn interactableConstructor interactableList acc =
            List.foldr (insertFn interactableConstructor) acc interactableList
    in
    Dict.empty
        |> foldFn item items
        |> foldFn location locations
        |> foldFn character characters


item : Interactable
item =
    Item False ItemOffScreen


location : Interactable
location =
    Location False


character : Interactable
character =
    Character CharacterOffScreen


getItemsInInventory : Manifest -> List String
getItemsInInventory manifest =
    let
        isInInventory ( id, interactable ) =
            case interactable of
                Item _ ItemInInventory ->
                    Just id

                _ ->
                    Nothing
    in
    Dict.toList manifest
        |> List.filterMap isInInventory


getLocations : Manifest -> List String
getLocations manifest =
    let
        isShownLocation ( id, interactable ) =
            case interactable of
                Location True ->
                    Just id

                _ ->
                    Nothing
    in
    Dict.toList manifest
        |> List.filterMap isShownLocation


getCharactersInLocation : String -> Manifest -> List String
getCharactersInLocation locationId manifest =
    let
        isInLocation ( id, interactable ) =
            case interactable of
                Character (CharacterInLocation location_) ->
                    if location_ == locationId then
                        Just id

                    else
                        Nothing

                _ ->
                    Nothing
    in
    Dict.toList manifest
        |> List.filterMap isInLocation


getItemsInLocation : String -> Manifest -> List String
getItemsInLocation locationId manifest =
    let
        isInLocation ( id, interactable ) =
            case interactable of
                Item _ (ItemInLocation location_) ->
                    if location_ == locationId then
                        Just id

                    else
                        Nothing

                _ ->
                    Nothing
    in
    Dict.toList manifest
        |> List.filterMap isInLocation


isItem : String -> Manifest -> Bool
isItem id manifest =
    Dict.get id manifest
        |> (\interactable ->
                case interactable of
                    Just (Item _ _) ->
                        True

                    _ ->
                        False
           )


isLocation : String -> Manifest -> Bool
isLocation id manifest =
    Dict.get id manifest
        |> (\interactable ->
                case interactable of
                    Just (Location _) ->
                        True

                    _ ->
                        False
           )


isCharacter : String -> Manifest -> Bool
isCharacter id manifest =
    Dict.get id manifest
        |> (\interactable ->
                case interactable of
                    Just (Character _) ->
                        True

                    _ ->
                        False
           )


update : ChangeWorldCommand -> Manifest -> Manifest
update change manifest =
    case change of
        MoveTo id ->
            Dict.update id addLocation manifest

        AddLocation id ->
            Dict.update id addLocation manifest

        RemoveLocation id ->
            Dict.update id removeLocation manifest

        MoveItemToInventory id ->
            Dict.update id moveItemToInventory manifest

        MoveItemToLocation itemId locationId ->
            Dict.update itemId (moveItemToLocation locationId) manifest

        MoveItemToLocationFixed itemId locationId ->
            Dict.update itemId (moveItemToLocationFixed locationId) manifest

        MoveItemOffScreen id ->
            Dict.update id moveItemOffScreen manifest

        MoveCharacterToLocation characterId locationId ->
            Dict.update characterId (moveCharacterToLocation locationId) manifest

        MoveCharacterOffScreen id ->
            Dict.update id moveCharacterOffScreen manifest

        _ ->
            manifest


addLocation : Maybe Interactable -> Maybe Interactable
addLocation interactable =
    case interactable of
        Just (Location _) ->
            Just (Location True)

        _ ->
            interactable


removeLocation : Maybe Interactable -> Maybe Interactable
removeLocation interactable =
    case interactable of
        Just (Location _) ->
            Just (Location False)

        _ ->
            interactable


moveItemToInventory : Maybe Interactable -> Maybe Interactable
moveItemToInventory interactable =
    case interactable of
        Just (Item False _) ->
            Just (Item False ItemInInventory)

        _ ->
            interactable


moveItemOffScreen : Maybe Interactable -> Maybe Interactable
moveItemOffScreen interactable =
    case interactable of
        Just (Item _ _) ->
            Just (Item False ItemOffScreen)

        _ ->
            interactable


moveItemToLocationFixed : String -> Maybe Interactable -> Maybe Interactable
moveItemToLocationFixed locationId interactable =
    case interactable of
        Just (Item _ _) ->
            Just (Item True (ItemInLocation locationId))

        _ ->
            interactable


moveItemToLocation : String -> Maybe Interactable -> Maybe Interactable
moveItemToLocation locationId interactable =
    case interactable of
        Just (Item _ _) ->
            Just (Item False (ItemInLocation locationId))

        _ ->
            interactable


moveCharacterToLocation : String -> Maybe Interactable -> Maybe Interactable
moveCharacterToLocation locationId interactable =
    case interactable of
        Just (Character _) ->
            Just (Character (CharacterInLocation locationId))

        _ ->
            interactable


moveCharacterOffScreen : Maybe Interactable -> Maybe Interactable
moveCharacterOffScreen interactable =
    case interactable of
        Just (Character _) ->
            Just (Character CharacterOffScreen)

        _ ->
            interactable


itemIsInInventory : String -> Manifest -> Bool
itemIsInInventory id manifest =
    getItemsInInventory manifest
        |> List.any ((==) id)


characterIsInLocation : String -> String -> Manifest -> Bool
characterIsInLocation character_ currentLocation manifest =
    getCharactersInLocation currentLocation manifest
        |> List.any ((==) character_)


itemIsInLocation : String -> String -> Manifest -> Bool
itemIsInLocation item_ currentLocation manifest =
    getItemsInLocation currentLocation manifest
        |> List.any ((==) item_)
