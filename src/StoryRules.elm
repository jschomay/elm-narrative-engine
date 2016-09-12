module StoryRules exposing (..)

import StoryState exposing (..)
import StoryElements exposing (..)


type alias Scene a b c d e =
    List (StoryRule a b c d e)


type alias StoryRule a b c d e =
    ( Given a b c e, AdvanceStory a b c d e )


type alias Given a b c e =
    ( Trigger a b c, Condition a b c e )


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


firstInteractionWith : StoryElement a b c -> Condition a b c e -> AdvanceStory a b c d e -> StoryRule a b c d e
firstInteractionWith storyElement condition advanceStory =
    ( ( FirstInteractionWith storyElement, condition ), advanceStory )


interactingWith : StoryElement a b c -> Condition a b c e -> AdvanceStory a b c d e -> StoryRule a b c d e
interactingWith storyElement condition advanceStory =
    ( ( InteractionWith storyElement, condition ), advanceStory )


when : (Condition a b c e -> AdvanceStory a b c d e -> StoryRule a b c d e) -> Condition a b c e -> AdvanceStory a b c d e -> StoryRule a b c d e
when f condition =
    f condition


changesWorld : (AdvanceStory a b c d e -> StoryRule a b c d e) -> ChangeWorldCommands a b c d e -> Narration -> StoryRule a b c d e
changesWorld f a b =
    f ( a, b )


narrates : (Narration -> StoryRule a b c d e) -> String -> StoryRule a b c d e
narrates f =
    f << Narrate


findMatchingRule : StoryElement a b c -> Scene a b c d e -> StoryState a b c d e -> Bool -> Maybe (AdvanceStory a b c d e)
findMatchingRule storyElement rules storyState beenThereDoneThat =
    case rules of
        [] ->
            Nothing

        (( given, advanceStory ) as x) :: xs ->
            if matchesGiven given storyElement storyState beenThereDoneThat then
                Just advanceStory
            else
                findMatchingRule storyElement xs storyState beenThereDoneThat


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
