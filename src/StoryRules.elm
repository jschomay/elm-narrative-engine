module StoryRules exposing (..)

import StoryState exposing (..)
import StoryElements exposing (..)


type alias SceneSelector a b c d e =
    d -> Scene a b c d e


type alias Scene a b c d e =
    List (StoryRule a b c d e)


type alias StoryRule a b c d e =
    ( Given a b c e, Do a b c d e )


type alias Given a b c e =
    ( Trigger a b c, Condition a b c e )


type alias Do a b c d e =
    ( ChangeWorldCommands a b c d e, Narration )


type alias ChangeWorldCommands a b c d e =
    List (ChangeWorldCommand a b c d e)


type Trigger a b c
    = InteractionWith (StoryElement a b c)
    | FirstInteractionWith (StoryElement a b c)


type Condition a b c e
    = Always
    | WithItem a
    | NearCharacter c
    | NearProp a
    | InLocation b
    | WithKnowledge e
    | All (List (Condition a b c e))
    | Any (List (Condition a b c e))
    | Not (Condition a b c e)


type ChangeWorldCommand a b c d e
    = MoveTo b
    | AddLocation b
    | RemoveLocation b
    | AddInventory a
    | RemoveInventory a
    | AddCharacter c b
    | RemoveCharacter c b
    | AddProp a b
    | RemoveProp a b
    | AddKnowledge e
    | LoadScene d
    | EndStory


type Narration
    = Narrate String


firstInteractionWith : StoryElement a b c -> Condition a b c e -> Do a b c d e -> StoryRule a b c d e
firstInteractionWith storyElement condition do =
    (( FirstInteractionWith storyElement, condition ), do)


interactingWith : StoryElement a b c -> Condition a b c e -> Do a b c d e -> StoryRule a b c d e
interactingWith storyElement condition do =
    (( InteractionWith storyElement, condition ), do)


when : (Condition a b c e -> Do a b c d e -> StoryRule a b c d e) -> Condition a b c e  -> Do a b c d e -> StoryRule a b c d e
when f condition =
    f condition


changesWorld : (Do a b c d e -> StoryRule a b c d e) -> ChangeWorldCommands a b c d e -> Narration -> StoryRule a b c d e
changesWorld f a b =
    f ( a, b )


narrates : (Narration -> StoryRule a b c d e) -> String -> StoryRule a b c d e
narrates f =
    f << Narrate


updateFromRules : StoryElement a b c -> Scene a b c d e -> StoryState a b c d e -> Bool -> String -> Maybe (StoryState a b c d e)
updateFromRules storyElement scene storyState beenThereDoneThat storyElementName =
    findFirstMatchingRule scene storyElement storyState beenThereDoneThat
        `Maybe.andThen` (Just << updateStoryState storyElementName storyState)


findFirstMatchingRule : Scene a b c d e -> StoryElement a b c -> StoryState a b c d e -> Bool -> Maybe (Do a b c d e)
findFirstMatchingRule rules storyElement storyState beenThereDoneThat =
    case rules of
        [] ->
            Nothing

        (( given, do ) as x) :: xs ->
            if matchesGiven given storyElement storyState beenThereDoneThat then
                Just do
            else
                findFirstMatchingRule xs storyElement storyState beenThereDoneThat


matchesGiven : Given a b c e -> StoryElement a b c -> StoryState a b c d e -> Bool -> Bool
matchesGiven ( trigger, condition ) storyElement storyState beenThereDoneThat =
    matchesTrigger trigger storyElement beenThereDoneThat && matchesCondition condition storyState


matchesTrigger : Trigger a b c -> StoryElement a b c -> Bool -> Bool
matchesTrigger trigger storyElement beenThereDoneThat =
    case trigger of
        InteractionWith storyElement' ->
            storyElement == storyElement'

        FirstInteractionWith storyElement' ->
            storyElement == storyElement' && not beenThereDoneThat


matchesCondition : Condition a b c e -> StoryState a b c d e -> Bool
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

        WithKnowledge knowledge ->
            List.member knowledge storyState.knowledge

        All conditions ->
            List.all (flip matchesCondition storyState) conditions

        Any conditions ->
            List.any (flip matchesCondition storyState) conditions

        Not condition ->
            not <| matchesCondition condition storyState


updateStoryState : String -> StoryState a b c d e -> Do a b c d e -> StoryState a b c d e
updateStoryState storyElementName storyState ( changesWorldCommands, narration ) =
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

                AddKnowledge knowledge ->
                    addKnowledge knowledge storyState

                LoadScene scene ->
                    setCurrentScene scene storyState

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( storyElementName, getNarration narration )
