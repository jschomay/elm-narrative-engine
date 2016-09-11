module StoryRules exposing (..)

import StoryState exposing (..)
import StoryElements exposing (..)


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
    = EveryTime
    | WithItem a
    | NearCharacter c
    | NearProp a
    | InLocation b
    | WithKnowledge e
    | All (List (Condition a b c e))
    | Any (List (Condition a b c e))
    | Unless (Condition a b c e)


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


everyTime : Condition a b c e
everyTime =
    EveryTime


withItem : a -> Condition a b c e
withItem =
    WithItem


nearCharacter : c -> Condition a b c e
nearCharacter =
    NearCharacter


nearProp : a -> Condition a b c e
nearProp =
    NearProp


inLocation : b -> Condition a b c e
inLocation =
    InLocation


withKnowledge : e -> Condition a b c e
withKnowledge =
    WithKnowledge


all : List (Condition a b c e) -> Condition a b c e
all =
    All


any : List (Condition a b c e) -> Condition a b c e
any =
    Any


unless : Condition a b c e -> Condition a b c e
unless =
    Unless


moveTo : b -> ChangeWorldCommand a b c d e
moveTo =
    MoveTo


addLocation : b -> ChangeWorldCommand a b c d e
addLocation =
    AddLocation


removeLocation : b -> ChangeWorldCommand a b c d e
removeLocation =
    RemoveLocation


addInventory : a -> ChangeWorldCommand a b c d e
addInventory =
    AddInventory


removeInventory : a -> ChangeWorldCommand a b c d e
removeInventory =
    RemoveInventory


addCharacter : c -> b -> ChangeWorldCommand a b c d e
addCharacter =
    AddCharacter


removeCharacter : c -> b -> ChangeWorldCommand a b c d e
removeCharacter =
    RemoveCharacter


addProp : a -> b -> ChangeWorldCommand a b c d e
addProp =
    AddProp


removeProp : a -> b -> ChangeWorldCommand a b c d e
removeProp =
    RemoveProp


addKnowledge : e -> ChangeWorldCommand a b c d e
addKnowledge =
    AddKnowledge


loadScene : d -> ChangeWorldCommand a b c d e
loadScene =
    LoadScene


endStory : ChangeWorldCommand a b c d e
endStory =
    EndStory


firstInteractionWith : StoryElement a b c -> Condition a b c e -> Do a b c d e -> StoryRule a b c d e
firstInteractionWith storyElement condition do =
    ( ( FirstInteractionWith storyElement, condition ), do )


interactingWith : StoryElement a b c -> Condition a b c e -> Do a b c d e -> StoryRule a b c d e
interactingWith storyElement condition do =
    ( ( InteractionWith storyElement, condition ), do )


when : (Condition a b c e -> Do a b c d e -> StoryRule a b c d e) -> Condition a b c e -> Do a b c d e -> StoryRule a b c d e
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
            storyElement == storyElement' && Basics.not beenThereDoneThat


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
                    StoryState.addLocation location storyState

                RemoveLocation location ->
                    StoryState.removeLocation location storyState

                AddInventory item ->
                    StoryState.addInventory item storyState

                RemoveInventory item ->
                    StoryState.removeInventory item storyState

                AddCharacter character location ->
                    StoryState.addCharacter character location storyState

                RemoveCharacter character location ->
                    StoryState.removeCharacter character location storyState

                AddProp prop location ->
                    StoryState.addProp prop location storyState

                RemoveProp prop location ->
                    StoryState.removeProp prop location storyState

                AddKnowledge knowledge ->
                    StoryState.addKnowledge knowledge storyState

                LoadScene scene ->
                    setCurrentScene scene storyState

                EndStory ->
                    storyState
    in
        List.foldl doCommand storyState changesWorldCommands
            |> addNarration ( storyElementName, getNarration narration )
