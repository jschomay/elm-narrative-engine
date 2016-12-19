module Engine.Manifest
    exposing
        ( init
        , item
        , location
        , character
        , getAttributes
        , getItemsInInventory
        , getLocations
        , getCharactersInLocation
        , getItemsInLocation
        , itemIsInInventory
        , characterIsPresent
        , itemIsPresent
        , isItem
        , isLocation
        , isCharacter
        , update
        )

import Types exposing (..)
import Dict exposing (Dict)


-- Model


init :
    { items : List ( String, Attributes )
    , locations : List ( String, Attributes )
    , characters : List ( String, Attributes )
    }
    -> Manifest
init { items, locations, characters } =
    let
        insertFn interactableConstructor ( id, attrs ) acc =
            Dict.insert id (interactableConstructor attrs) acc

        foldFn interactableConstructor interactableList acc =
            List.foldr (insertFn interactableConstructor) acc interactableList
    in
        Dict.empty
            |> foldFn item items
            |> foldFn location locations
            |> foldFn character characters


item : Attributes -> Interactable
item attrs =
    Item ItemOffScreen attrs


location : Attributes -> Interactable
location attrs =
    Location False attrs


character : Attributes -> Interactable
character attrs =
    Character CharacterOffScreen attrs


getAttributes : String -> Manifest -> Maybe Attributes
getAttributes id manifest =
    let
        getAttrs interactable =
            case interactable of
                Item _ attrs ->
                    attrs

                Location _ attrs ->
                    attrs

                Character _ attrs ->
                    attrs
    in
        Dict.get id manifest
            |> Maybe.map getAttrs


getItemsInInventory : Manifest -> List ( String, Attributes )
getItemsInInventory manifest =
    let
        isInInventory ( id, interactable ) =
            case interactable of
                Item ItemInInventory attrs ->
                    Just ( id, attrs )

                _ ->
                    Nothing
    in
        Dict.toList manifest
            |> List.filterMap isInInventory


getLocations : Manifest -> List ( String, Attributes )
getLocations manifest =
    let
        isShownLocation ( id, interactable ) =
            case interactable of
                Location True attrs ->
                    Just ( id, attrs )

                _ ->
                    Nothing
    in
        Dict.toList manifest
            |> List.filterMap isShownLocation


getCharactersInLocation : String -> Manifest -> List ( String, Attributes )
getCharactersInLocation locationId manifest =
    let
        isInLocation ( id, interactable ) =
            case interactable of
                Character (CharacterInLocation location) attrs ->
                    if location == locationId then
                        Just ( id, attrs )
                    else
                        Nothing

                _ ->
                    Nothing
    in
        Dict.toList manifest
            |> List.filterMap isInLocation


getItemsInLocation : String -> Manifest -> List ( String, Attributes )
getItemsInLocation locationId manifest =
    let
        isInLocation ( id, interactable ) =
            case interactable of
                Item (ItemInLocation location) attrs ->
                    if location == locationId then
                        Just ( id, attrs )
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
        |> \interactable ->
            case interactable of
                Just (Item _ _) ->
                    True

                _ ->
                    False


isLocation : String -> Manifest -> Bool
isLocation id manifest =
    Dict.get id manifest
        |> \interactable ->
            case interactable of
                Just (Location _ _) ->
                    True

                _ ->
                    False


isCharacter : String -> Manifest -> Bool
isCharacter id manifest =
    Dict.get id manifest
        |> \interactable ->
            case interactable of
                Just (Character _ _) ->
                    True

                _ ->
                    False



-- Update


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

        MoveItem itemId locationId ->
            Dict.update itemId (moveItem locationId) manifest

        MoveItemOffScreen id ->
            Dict.update id moveItemOffScreen manifest

        MoveCharacter characterId locationId ->
            Dict.update characterId (moveCharacter locationId) manifest

        MoveCharacterOffScreen id ->
            Dict.update id moveCharacterOffScreen manifest

        _ ->
            manifest


addLocation : Maybe Interactable -> Maybe Interactable
addLocation interactable =
    case interactable of
        Just (Location _ attrs) ->
            Just (Location True attrs)

        _ ->
            Nothing


removeLocation : Maybe Interactable -> Maybe Interactable
removeLocation interactable =
    case interactable of
        Just (Location _ attrs) ->
            Just (Location False attrs)

        _ ->
            Nothing


moveItemToInventory : Maybe Interactable -> Maybe Interactable
moveItemToInventory interactable =
    case interactable of
        Just (Item _ attrs) ->
            Just (Item ItemInInventory attrs)

        _ ->
            Nothing


moveItemOffScreen : Maybe Interactable -> Maybe Interactable
moveItemOffScreen interactable =
    case interactable of
        Just (Item _ attrs) ->
            Just (Item ItemOffScreen attrs)

        _ ->
            Nothing


moveItem : String -> Maybe Interactable -> Maybe Interactable
moveItem locationId interactable =
    case interactable of
        Just (Item _ attrs) ->
            Just (Item (ItemInLocation locationId) attrs)

        _ ->
            Nothing


moveCharacter : String -> Maybe Interactable -> Maybe Interactable
moveCharacter locationId interactable =
    case interactable of
        Just (Character _ attrs) ->
            Just (Character (CharacterInLocation locationId) attrs)

        _ ->
            Nothing


moveCharacterOffScreen : Maybe Interactable -> Maybe Interactable
moveCharacterOffScreen interactable =
    case interactable of
        Just (Character _ attrs) ->
            Just (Character CharacterOffScreen attrs)

        _ ->
            Nothing


itemIsInInventory : String -> Manifest -> Bool
itemIsInInventory id manifest =
    getItemsInInventory manifest
        |> List.any (fst >> (==) id)


characterIsPresent : String -> String -> Manifest -> Bool
characterIsPresent character currentLocation manifest =
    getCharactersInLocation currentLocation manifest
        |> List.any (fst >> (==) character)


itemIsPresent : String -> String -> Manifest -> Bool
itemIsPresent item currentLocation manifest =
    getItemsInLocation currentLocation manifest
        |> List.any (fst >> (==) item)
