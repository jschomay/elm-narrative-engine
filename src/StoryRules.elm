module StoryRules exposing (..)

import StoryState exposing (..)


type alias Scene a =
    List (StoryRule a)


type StoryRule a
    = StoryRule (Given a) (Do a)


type Given a
    = Given (Trigger a) (Condition a)


type Do a
    = Do (List (ChangeWorldCommand a)) NarrateCommand


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


type ChangeWorldCommand a
    = MoveTo a
    | AddLocation a
    | RemoveLocation a
    | AddInventory a
    | RemoveInventory a
    | AddCharacter a
    | RemoveCharacter a
    | AddProp a
    | RemoveProp a



-- todo, fit StoryCommand into the Do along with ChangeWorldCommand and NarrateCommand


type StoryCommand a
    = LoadScene a
    | EndStory


type NarrateCommand
    = Narrate DisplayText


type DisplayText
    = Simple String
    | InOrder (List String)


updateFromRules : a -> Scene a -> StoryState a -> Maybe (StoryState a)
updateFromRules tag currentRules storyState =
    findFirstMatchingRule currentRules tag
        `Maybe.andThen` (Just << updateStoryState storyState)


findFirstMatchingRule : Scene a -> a -> Maybe (Do a)
findFirstMatchingRule rules tag =
    case rules of
        [] ->
            Nothing

        ((StoryRule given do) as x) :: xs ->
            if matchesGiven given tag then
                Just do
            else
                findFirstMatchingRule xs tag


matchesGiven : Given a -> a -> Bool
matchesGiven (Given trigger condition) tag =
    matchesTrigger trigger tag && matchesCondition condition tag


matchesTrigger : Trigger a -> a -> Bool
matchesTrigger trigger tag =
    case trigger of
        InteractionWith tagToMatch ->
            tagToMatch == tag

        _ ->
            False


matchesCondition : Condition a -> a -> Bool
matchesCondition condition tag =
    case condition of
        Always ->
            True

        WithOut iventory ->
            True

        _ ->
            False


updateStoryState : StoryState a -> Do a -> StoryState a
updateStoryState storyState (Do changeWorldCommands narrateCommand) =
    let
        addNarration (Narrate narration) =
            case narration of
                Simple t ->
                    t

                _ ->
                    "Other DisplayText not implmented"

        doCommand command storyState =
            case command of
                AddInventory item ->
                    { storyState
                        | inventory = item :: storyState.inventory
                        , storyLine = (addNarration narrateCommand) :: storyState.storyLine
                    }

                _ ->
                    storyState
    in
        List.foldl doCommand storyState changeWorldCommands
