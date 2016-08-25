module StoryRules exposing (..)

import StoryState exposing (..)


type alias StoryRulesConfig a b =
    b -> Scene a b


type alias Scene a b =
    List (StoryRule a b)


type alias StoryRule a b =
    ( Given a, Do a b )


type alias Given a =
    ( Trigger a, Condition a )


type alias Do a b =
    ( ChangeWorldCommands a b, Narration )


type alias ChangeWorldCommands a b =
    List (ChangeWorldCommand a b)


type Trigger a
    = InteractionWith a
    | FirstInteractionWith a


type Condition a
    = Always
    | WithItem a
    | NearCharacter a
    | NearProp a
    | InLocation a
    | All (List (Condition a))
    | Any (List (Condition a))
    | Not (Condition a)


type ChangeWorldCommand a b
    = MoveTo a
    | AddLocation a
    | RemoveLocation a
    | AddInventory a
    | RemoveInventory a
    | AddCharacter a a
    | RemoveCharacter a a
    | AddProp a a
    | RemoveProp a a
    | LoadScene b
    | EndStory


type Narration
    = Simple String
    | InOrder (List String)


given : Trigger a -> Condition a -> Do a b -> StoryRule a b
given trigger condition =
    (,) ( trigger, condition )


do : (Do a b -> StoryRule a b) -> ChangeWorldCommands a b -> Narration -> StoryRule a b
do f a b =
    f ( a, b )


narrate : (Narration -> StoryRule a b) -> Narration -> StoryRule a b
narrate f a =
    f a


updateFromRules : a -> StoryRulesConfig a b -> StoryState a b -> (a -> Bool) -> Maybe (StoryState a b)
updateFromRules storyElement storyRules storyState beenThereDoneThat =
    findFirstMatchingRule (storyRules storyState.currentScene) storyElement storyState beenThereDoneThat
        `Maybe.andThen` (Just << updateStoryState storyElement storyState)


findFirstMatchingRule : Scene a b -> a -> StoryState a b -> (a -> Bool) -> Maybe (Do a b)
findFirstMatchingRule rules storyElement storyState beenThereDoneThat =
    case rules of
        [] ->
            Nothing

        (( given, do ) as x) :: xs ->
            if matchesGiven given storyElement storyState beenThereDoneThat then
                Just do
            else
                findFirstMatchingRule xs storyElement storyState beenThereDoneThat


matchesGiven : Given a -> a -> StoryState a b -> (a -> Bool) -> Bool
matchesGiven ( trigger, condition ) storyElement storyState beenThereDoneThat =
    matchesTrigger trigger storyElement beenThereDoneThat && matchesCondition condition storyState


matchesTrigger : Trigger a -> a -> (a -> Bool) -> Bool
matchesTrigger trigger storyElement beenThereDoneThat =
    case trigger of
        InteractionWith storyElementToMatch ->
            storyElementToMatch == storyElement

        FirstInteractionWith storyElementToMatch ->
            storyElementToMatch == storyElement && (not <| beenThereDoneThat storyElement)


matchesCondition : Condition a -> StoryState a b -> Bool
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


updateStoryState : a -> StoryState a b -> Do a b -> StoryState a b
updateStoryState storyElement storyState ( changeWorldCommands, narration ) =
    let
        getNarration narration =
            case narration of
                Simple t ->
                    t

                _ ->
                    Debug.crash "Other DisplayText not implmented"

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
            |> addNarration ( storyElement, getNarration narration )
