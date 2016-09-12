module Story.Rule exposing (..)

import Story.State exposing (..)
import Story.Element exposing (..)


type alias Scene a b c d e =
    List (Rule a b c d e)


type alias Rule a b c d e =
    ( Given a b c e, AdvanceStory a b c d e )


type alias Given a b c e =
    ( Trigger a b c, Condition a b c e )


type Trigger a b c
    = InteractionWith (Element a b c)
    | FirstInteractionWith (Element a b c)


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


firstInteractionWith : Element a b c -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
firstInteractionWith element condition advanceStory =
    ( ( FirstInteractionWith element, condition ), advanceStory )


interactingWith : Element a b c -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
interactingWith element condition advanceStory =
    ( ( InteractionWith element, condition ), advanceStory )


when : (Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e) -> Condition a b c e -> AdvanceStory a b c d e -> Rule a b c d e
when f condition =
    f condition


changesWorld : (AdvanceStory a b c d e -> Rule a b c d e) -> ChangeWorldCommands a b c d e -> Narration -> Rule a b c d e
changesWorld f a b =
    f ( a, b )


narrates : (Narration -> Rule a b c d e) -> String -> Rule a b c d e
narrates f =
    f << Narrate


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
