module StoryRules exposing (..)

import StoryState exposing (..)
import StoryElements exposing (..)


type alias SceneSelector a b c d =
    d -> Scene a b c d


type alias Scene a b c d =
    List (StoryRule a b c d)


type alias StoryRule a b c d =
    ( Given a b c, Do a b c d )


type alias Given a b c =
    ( Trigger a b c, Condition a b c )


type alias Do a b c d =
    ( ChangeWorldCommands a b c d, Narration )


type alias ChangeWorldCommands a b c d =
    List (ChangeWorldCommand a b c d)


type Trigger a b c
    = InteractionWithItem a
    | InteractionWithLocation b
    | InteractionWithCharacter c
    | FirstInteractionWithItem a
    | FirstInteractionWithLocation b
    | FirstInteractionWithCharacter c


type Condition a b c
    = Always
    | WithItem a
    | NearCharacter c
    | NearProp a
    | InLocation b
    | All (List (Condition a b c))
    | Any (List (Condition a b c))
    | Not (Condition a b c)


type ChangeWorldCommand a b c d
    = MoveTo b
    | AddLocation b
    | RemoveLocation b
    | AddInventory a
    | RemoveInventory a
    | AddCharacter c b
    | RemoveCharacter c b
    | AddProp a b
    | RemoveProp a b
    | LoadScene d
    | EndStory


type Narration
    = Narrate String


given : Trigger a b c -> Condition a b c -> Do a b c d -> StoryRule a b c d
given trigger condition =
    (,) ( trigger, condition )


changeWorld : (Do a b c d -> StoryRule a b c d) -> ChangeWorldCommands a b c d -> Narration -> StoryRule a b c d
changeWorld f a b =
    f ( a, b )


narrate : (Narration -> StoryRule a b c d) -> String -> StoryRule a b c d
narrate f a =
    f <| Narrate a


updateFromRules : StoryElement a b c -> Scene a b c d -> StoryState a b c d -> Bool -> String -> Maybe (StoryState a b c d)
updateFromRules storyElement scene storyState beenThereDoneThat storyElementName =
    findFirstMatchingRule scene storyElement storyState beenThereDoneThat
        `Maybe.andThen` (Just << updateStoryState storyElementName storyState)


findFirstMatchingRule : Scene a b c d -> StoryElement a b c -> StoryState a b c d -> Bool -> Maybe (Do a b c d)
findFirstMatchingRule rules storyElement storyState beenThereDoneThat =
    case rules of
        [] ->
            Nothing

        (( given, do ) as x) :: xs ->
            if matchesGiven given storyElement storyState beenThereDoneThat then
                Just do
            else
                findFirstMatchingRule xs storyElement storyState beenThereDoneThat


matchesGiven : Given a b c -> StoryElement a b c -> StoryState a b c d -> Bool -> Bool
matchesGiven ( trigger, condition ) storyElement storyState beenThereDoneThat =
    matchesTrigger trigger storyElement beenThereDoneThat && matchesCondition condition storyState


matchesTrigger : Trigger a b c -> StoryElement a b c -> Bool -> Bool
matchesTrigger trigger storyElement beenThereDoneThat =
    case ( storyElement, trigger ) of
        ( Item item, InteractionWithItem item' ) ->
            item == item'

        ( Item item, FirstInteractionWithItem item' ) ->
            item == item' && not beenThereDoneThat

        ( Location location, InteractionWithLocation location' ) ->
            location == location'

        ( Location location, FirstInteractionWithLocation location' ) ->
            location == location' && not beenThereDoneThat

        ( Character character, InteractionWithCharacter character' ) ->
            character == character'

        ( Character character, FirstInteractionWithCharacter character' ) ->
            character == character' && not beenThereDoneThat

        _ ->
            False


matchesCondition : Condition a b c -> StoryState a b c d -> Bool
matchesCondition condition storyState =
    case condition of
        Always ->
            True

        WithItem item ->
            List.member item storyState.inventory

        NearCharacter character ->
            List.member character <| getCharactersInCurrentLocation storyState

        NearProp prop ->
            List.member prop <| getPropsInCurrentLocation storyState

        InLocation location ->
            storyState.currentLocation == location

        All conditions ->
            List.all (flip matchesCondition storyState) conditions

        Any conditions ->
            List.any (flip matchesCondition storyState) conditions

        Not condition ->
            not <| matchesCondition condition storyState


updateStoryState : String -> StoryState a b c d -> Do a b c d -> StoryState a b c d
updateStoryState storyElementName storyState ( changeWorldCommands, narration ) =
    let
        getNarration narration =
            case narration of
                Narrate t ->
                    t

        doCommand command storyState =
            case command of
                MoveTo location ->
                    setCurrentLocation location storyState

                AddLocation location ->
                    addLocation location storyState

                RemoveLocation location ->
                    removeLocation location storyState

                AddInventory item ->
                    addInventory item storyState

                RemoveInventory item ->
                    removeInventory item storyState

                AddCharacter character location ->
                    addCharacter character location storyState

                RemoveCharacter character location ->
                    removeCharacter character location storyState

                AddProp prop location ->
                    addProp prop location storyState

                RemoveProp prop location ->
                    removeProp prop location storyState

                LoadScene scene ->
                    setCurrentScene scene storyState

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changeWorldCommands
            |> addNarration ( storyElementName, getNarration narration )
