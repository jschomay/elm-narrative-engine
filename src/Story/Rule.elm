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
