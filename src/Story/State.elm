module Story.State exposing (..)

import Dict exposing (..)
import Story.Element exposing (..)


-- Rule


type alias Rule item location character knowledge =
    { interaction : Interaction item location character
    , condition : List (Condition item location character knowledge)
    , changes : List (ChangeWorldCommand item location character knowledge)
    , narration : Narration
    }


type Interaction a b c
    = InteractionBased Time (Element a b c)
    | TurnBased Time


withCharacter : character -> Trigger item location character
withCharacter character =
    InteractionBased (Time { before = Nothing, after = Nothing }) (Character character)


firstWithCharacter : character -> Trigger item location character
firstWithCharacter character =
    InteractionBased (Time { before = Just 2, after = Nothing }) (Character character)


type Time
    = Time { before : Maybe Int, after : Maybe Int }


type Condition a b c e
    = WithItem a
    | NearCharacter c
    | NearProp a
    | InLocation b
    | WithKnowledge e
    | Any (List (Condition a b c e))
    | Unless (Condition a b c e)


type ChangeWorldCommand item location character knowledge
    = MoveTo location
    | AddLocation location
    | RemoveLocation location
    | AddInventory item
    | RemoveInventory item
    | AddCharacter character location
    | RemoveCharacter character location
    | AddProp item location
    | RemoveProp item location
    | AddKnowledge knowledge
    | LoadScene (List (Rule item location character knowledge))
    | EndStory


type Narration
    = Narrate String
    | InOrder (List String)
    | Cycling (List String)



-- StoryState


type alias StoryState a b c d e =
    { currentLocation : b
    , currentScene : d
    , familiarWith : List (Element a b c)
    , inventory : List a
    , knownLocations : List b
    , storyLine : List ( String, String )
    , itemsByLocation : Dict String (List a)
    , charactersByLocation : Dict String (List c)
    , knowledge : List e
    }


init : b -> d -> StoryState a b c d e
init startingLocation startingScene =
    StoryState startingLocation startingScene [ Location startingLocation ] [] [] [] Dict.empty Dict.empty []


getCharactersInCurrentLocation : StoryState a b c d e -> List c
getCharactersInCurrentLocation storyState =
    getCharactersByLocation storyState.currentLocation storyState


getPropsInCurrentLocation : StoryState a b c d e -> List a
getPropsInCurrentLocation storyState =
    getItemsByLocation storyState.currentLocation storyState


getCharactersByLocation : b -> StoryState a b c d e -> List c
getCharactersByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.charactersByLocation


getItemsByLocation : b -> StoryState a b c d e -> List a
getItemsByLocation location storyState =
    Maybe.withDefault []
        <| Dict.get (toString location) storyState.itemsByLocation


advanceStory : String -> StoryState a b c d e -> AdvanceStory a b c d e -> StoryState a b c d e
advanceStory elementName storyState ( changesWorldCommands, narration ) =
    let
        getNarration narration =
            case narration of
                Narrate t ->
                    t

        addNarration narration storyState =
            { storyState
                | storyLine = narration :: storyState.storyLine
            }

        doCommand command storyState =
            case command of
                MoveTo location ->
                    if List.member (Location location) storyState.familiarWith then
                        { storyState
                            | currentLocation = location
                        }
                    else
                        { storyState
                            | currentLocation = location
                            , familiarWith = storyState.familiarWith ++ [ Location location ]
                        }

                AddLocation location ->
                    if List.member location storyState.knownLocations then
                        storyState
                    else
                        { storyState
                            | knownLocations = location :: storyState.knownLocations
                        }

                RemoveLocation location ->
                    { storyState
                        | knownLocations = List.filter ((/=) location) storyState.knownLocations
                    }

                AddInventory item ->
                    if List.member item storyState.inventory then
                        storyState
                    else
                        { storyState
                            | inventory = item :: storyState.inventory
                        }

                RemoveInventory item ->
                    { storyState
                        | inventory = List.filter ((/=) item) storyState.inventory
                    }

                AddCharacter character location ->
                    if List.member character (getCharactersByLocation location storyState) then
                        storyState
                    else
                        { storyState
                            | charactersByLocation =
                                Dict.insert (toString location)
                                    ((getCharactersByLocation location storyState) ++ [ character ])
                                    storyState.charactersByLocation
                        }

                RemoveCharacter character location ->
                    if not <| Dict.member (toString location) storyState.charactersByLocation then
                        storyState
                    else
                        { storyState
                            | charactersByLocation =
                                Dict.insert (toString location)
                                    (getCharactersByLocation location storyState
                                        |> List.filter ((/=) character)
                                    )
                                    storyState.charactersByLocation
                        }

                AddProp prop location ->
                    if List.member prop (getItemsByLocation location storyState) then
                        storyState
                    else
                        { storyState
                            | itemsByLocation =
                                Dict.insert (toString location)
                                    ((getItemsByLocation location storyState) ++ [ prop ])
                                    storyState.itemsByLocation
                        }

                RemoveProp prop location ->
                    if not <| Dict.member (toString location) storyState.itemsByLocation then
                        storyState
                    else
                        { storyState
                            | itemsByLocation =
                                Dict.insert (toString location)
                                    (getItemsByLocation location storyState
                                        |> List.filter ((/=) prop)
                                    )
                                    storyState.itemsByLocation
                        }

                AddKnowledge knowledge ->
                    if List.member knowledge storyState.knowledge then
                        storyState
                    else
                        { storyState
                            | knowledge = knowledge :: storyState.knowledge
                        }

                LoadScene scene ->
                    { storyState
                        | currentScene = scene
                    }

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( elementName, getNarration narration )


findMatchingRule : Element a b c -> Scene a b c d e -> StoryState a b c d e -> Bool -> Maybe (AdvanceStory a b c d e)
findMatchingRule element rules storyState beenThereDoneThat =
    case rules of
        [] ->
            Nothing

        (( given, advanceStory ) as x) :: xs ->
            if matchesGiven given element storyState beenThereDoneThat then
                Just advanceStory
            else
                findMatchingRule element xs storyState beenThereDoneThat


matchesGiven : Given a b c e -> Element a b c -> StoryState a b c d e -> Bool -> Bool
matchesGiven ( trigger, condition ) element storyState beenThereDoneThat =
    matchesTrigger trigger element beenThereDoneThat && matchesCondition condition storyState


matchesTrigger : Trigger a b c -> Element a b c -> Bool -> Bool
matchesTrigger trigger element beenThereDoneThat =
    case trigger of
        InteractionWith element' ->
            element == element'

        FirstInteractionWith element' ->
            element == element' && Basics.not beenThereDoneThat


matchesCondition : Condition a b c e -> StoryState a b c d e -> Bool
matchesCondition condition storyState =
    case condition of
        EveryTime ->
            True

        WithItem item ->
            List.member item storyState.inventory

        NearCharacter character ->
            List.member character <| getCharactersInCurrentLocation storyState

        NearProp prop ->
            List.member prop <| getPropsInCurrentLocation storyState

        InLocation location ->
            storyState.currentLocation == location

        WithKnowledge knowledge ->
            List.member knowledge storyState.knowledge

        All conditions ->
            List.all (flip matchesCondition storyState) conditions

        Any conditions ->
            List.any (flip matchesCondition storyState) conditions

        Unless condition ->
            Basics.not <| matchesCondition condition storyState
