module StoryRules exposing (..)

import StoryState exposing (..)


type alias StoryRulesConfig a b =
    b -> Scene a b


type alias Scene a b =
    List (StoryRule a b)


type StoryRule a b
    = StoryRule (Given a) (Do a b)


type Given a
    = Given (Trigger a) (Condition a)


type Do a b
    = Do (List (ChangeWorldCommand a b)) NarrateCommand


type Trigger a
    = EveryTurn
    | AfterTurn Int
    | UntilTurn Int
    | OnTurn Int
    | InteractionWith a


type Condition a
    = Always
    | With (List a)
    | WithOut (List a)
    | Near (List a)
    | NotNear (List a)
    | In (List a)
    | NotIn (List a)
    | And (Condition a) (Condition a)
    | Or (Condition a) (Condition a)


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


type NarrateCommand
    = Narrate DisplayText


type DisplayText
    = Simple String
    | InOrder (List String)


updateFromRules : a -> StoryRulesConfig a b -> StoryState a b -> Maybe (StoryState a b)
updateFromRules storyElement storyRules storyState =
    findFirstMatchingRule (storyRules storyState.currentScene) storyElement
        `Maybe.andThen` (Just << updateStoryState storyState)


findFirstMatchingRule : Scene a b -> a -> Maybe (Do a b)
findFirstMatchingRule rules storyElement =
    case rules of
        [] ->
            Nothing

        ((StoryRule given do) as x) :: xs ->
            if matchesGiven given storyElement then
                Just do
            else
                findFirstMatchingRule xs storyElement


matchesGiven : Given a -> a -> Bool
matchesGiven (Given trigger condition) storyElement =
    matchesTrigger trigger storyElement && matchesCondition condition storyElement


matchesTrigger : Trigger a -> a -> Bool
matchesTrigger trigger storyElement =
    case trigger of
        InteractionWith storyElementToMatch ->
            storyElementToMatch == storyElement

        _ ->
            False


matchesCondition : Condition a -> a -> Bool
matchesCondition condition storyElement =
    case condition of
        Always ->
            True

        WithOut iventory ->
            True

        _ ->
            False


updateStoryState : StoryState a b -> Do a b -> StoryState a b
updateStoryState storyState (Do changeWorldCommands narrateCommand) =
    let
        getNarration (Narrate displayText) =
            case displayText of
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
            |> addNarration (getNarration narrateCommand)
