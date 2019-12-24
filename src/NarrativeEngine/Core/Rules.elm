module NarrativeEngine.Core.Rules exposing (Rule, RuleID, Rules, findMatchingRule, weight)

{-| Rules are a declarative way of describing meaningful events in your story.

They are made up of 3 parts: a "trigger", a set of "conditions", and a set of "changes" to apply if the rule matches. Each time you call `findMatchingRule`, the engine will test all of your rules against the provided trigger and the current state of your story world, and will find the best matching rule (if one exists).

Rules are weighted based on how specific they are, so you can "override" a more generic rule by making a more specific rule that will also match.

It is possible to create generic rules (using `MatchAny`) to control basic story logic, and more specific rules to flesh out the story.

See how the rules are defined in the [full working example](https://github.com/jschomay/elm-narrative-engine/blob/master/src/Example.elm).

@docs Rule, RuleID, Rules, findMatchingRule, weight

-}

import Dict exposing (Dict)
import NarrativeEngine.Core.WorldModel as WorldModel exposing (ChangeWorld, EntityMatcher(..), Query, WorldModel)


{-| Unique ID for a rule. These ids will be returned when a matching rule is found.
-}
type alias RuleID =
    String


{-| All of the rules in your "rule book" that define your game logic.
-}
type alias Rules a =
    Dict RuleID (Rule a)


{-| A declarative rule describing the conditions in which it should apply. Specifically, it defines a trigger and a list of other conditions that must be present. All of these are described by `EntityMatcher`s. All rules are tested on each player interaction, and the highest weighted matching rule will be returned. You can then apply the specified changes.

Note that `Rule`s are extensible records, meaning that you can add other fields to them in your game. For example, you could add a `narrative` field to add a story text to use when the rule matches, or a `sound` to play, etc. All of these "side effects" would be handled in your game code. Alternatively, you could use the returned `RuleID` to lookup side effects in a separate data structure. This design follows the Entity Component System (ECS) design pattern.

-}
type alias Rule a =
    { a
        | trigger : EntityMatcher
        , conditions : List EntityMatcher
        , changes : List ChangeWorld
    }


{-| Finds the rule that best matches against the provided trigger (entity ID) and current world model. If multiple rules match, this chooses the "best" match based on the most _specific_ rule. In general, the more conditions, the more specific.

Call this any time the player "interacts" with an entity in your game, supplying the ID of the entity that was interacted with.

-}
findMatchingRule : WorldModel.ID -> Rules a -> WorldModel b -> Maybe ( RuleID, Rule a )
findMatchingRule trigger rules store =
    rules
        |> Dict.filter
            (\ruleId rule ->
                matchTrigger store trigger rule.trigger
                    && List.all (matchCondition trigger store) rule.conditions
            )
        |> Dict.toList
        |> List.sortBy (Tuple.second >> weight)
        |> List.reverse
        |> List.head


matchTrigger : WorldModel a -> WorldModel.ID -> EntityMatcher -> Bool
matchTrigger store trigger matcher =
    WorldModel.replaceTrigger trigger matcher
        |> (\m ->
                case m of
                    Match id qs ->
                        (id == trigger)
                            && (WorldModel.query m store
                                    |> List.isEmpty
                                    |> not
                               )

                    MatchAny qs ->
                        WorldModel.query (Match trigger qs) store |> List.isEmpty |> not
           )


matchCondition : WorldModel.ID -> WorldModel a -> EntityMatcher -> Bool
matchCondition trigger store matcher =
    WorldModel.replaceTrigger trigger matcher
        |> (\m -> WorldModel.query m store)
        |> List.isEmpty
        |> not


{-| Assigns a "weighting" to a rule. Used internally.
-}
weight : Rule a -> Int
weight { trigger, conditions } =
    let
        queryScore matcher =
            case matcher of
                Match id queries ->
                    List.length queries

                MatchAny queries ->
                    List.length queries

        conditionsScore =
            conditions
                |> List.foldl
                    (\matcher acc ->
                        case matcher of
                            Match _ _ ->
                                10 + queryScore matcher + acc

                            MatchAny _ ->
                                0 + queryScore matcher + acc
                    )
                    0

        triggerScore =
            case trigger of
                Match _ _ ->
                    100 + queryScore trigger

                MatchAny _ ->
                    0 + queryScore trigger
    in
    conditionsScore + triggerScore
