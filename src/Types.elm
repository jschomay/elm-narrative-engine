module Types exposing (..)

import Color
import EveryDict exposing (..)
import List.Zipper


-- msg


type Msg item location character
    = NoOp
    | StartGame
    | Interact (Interactable item location character)
    | Rollback Int



-- StoryState


type alias StoryHistory item location character knowledge =
    { interactions : List (Interaction item location character)
    , startingState : StoryState item location character knowledge
    }


type Interaction item location character
    = Interaction (Interactable item location character)


type alias StoryState item location character knowledge =
    { currentLocation : location
    , currentScene : List (LiveRule item location character knowledge)
    , familiarWith : List (Interactable item location character)
    , knownLocations : List location
    , storyLine : List ( String, String )
    , characterPlacements : EveryDict character location
    , itemPlacements : EveryDict item (ItemPlacement location)
    , knowledge : List knowledge
    }


type ItemPlacement location
    = Prop location
    | Inventory



-- Rules


type alias RuleIndex =
    Int


type alias Rule item location character knowledge =
    { interaction : Interactable item location character
    , conditions : List (Condition item location character knowledge)
    , changes : List (ChangeWorldCommand item location character knowledge)
    , narration : List String
    }


type alias LiveRule item location character knowledge =
    { interaction : Interactable item location character
    , conditions : List (Condition item location character knowledge)
    , changes : List (ChangeWorldCommand item location character knowledge)
    , narration : Maybe (List.Zipper.Zipper String)
    }


loadCurrentScene : List (Rule item location character knowledge) -> List (LiveRule item location character knowledge)
loadCurrentScene ruleData =
    let
        toLiveRule rule =
            LiveRule rule.interaction rule.conditions rule.changes (List.Zipper.fromList rule.narration)
    in
        List.map toLiveRule ruleData


type Condition item location character knowledge
    = WithItem item
    | NearCharacter character
    | NearItem item
    | InLocation location
    | WithKnowledge knowledge
    | Unless (Condition item location character knowledge)


type ChangeWorldCommand item location character knowledge
    = MoveTo location
    | AddLocation location
    | RemoveLocation location
    | AddInventory item
    | RemoveInventory item
    | MoveCharacter character location
    | RemoveCharacter character
    | PlaceItem item location
    | RemoveItem item
    | AddKnowledge knowledge
    | LoadScene (List (Rule item location character knowledge))
    | EndStory



-- Interactables


type Interactable item location character
    = Item item
    | Location location
    | Character character


type alias StoryWorld item location character =
    { items : item -> ItemInfo
    , locations : location -> LocationInfo
    , characters : character -> CharacterInfo
    }


type alias BasicInfo =
    { name : String
    , description : String
    }


type alias WithColor a =
    { a | color : Color.Color }


type alias ItemInfo =
    BasicInfo


type alias LocationInfo =
    WithColor BasicInfo


type alias CharacterInfo =
    BasicInfo


getName : StoryWorld item location character -> Interactable item location character -> String
getName displayInfo interactable =
    case interactable of
        Item item ->
            .name <| displayInfo.items item

        Location location ->
            .name <| displayInfo.locations location

        Character character ->
            .name <| displayInfo.characters character


getDescription : StoryWorld item location character -> Interactable item location character -> String
getDescription displayInfo interactable =
    case interactable of
        Item item ->
            .description <| displayInfo.items item

        Location location ->
            .description <| displayInfo.locations location

        Character character ->
            .description <| displayInfo.characters character


getNarration : String -> LiveRule item location character knowledge -> String
getNarration default matchedRule =
    matchedRule.narration
        |> Maybe.map List.Zipper.current
        |> Maybe.withDefault default