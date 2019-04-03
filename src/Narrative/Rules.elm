module Narrative.Rules exposing
    ( EntityID
    , EntityMatcher(..)
    , Rule
    , RuleID
    , Rules
    , findMatchingRule
    , weight
    )

{-| Rules are a declarative way of describing meaningful events in your story.

They are made up of two parts: a "trigger", and a set of "conditions". Each time you call `findMatchingRule`, the engine will test all of your rules against the provided trigger and the current state of your story world, and will find the best matching rule (if one exists).

Because `Rule`s are extended records, you can also include other useful data on them, such as how the state of your story world should change, or which specific narrative to show, or which sound effect to play, etc. The engine will ignore these fields, but your client can act on them when a rule matches.

@doc EntityID, RuleID, Rules, Rule, EntityMatcher(..), Condition(..), findMatchingRule

Example rules using the example store in `Narrative.WorldModel`:

TODO show examples of generic trigger and condition and trigger queries (use locked door examples on trello card)
TODO document generic conditions
TODO document weighting
TODO document trigger matching with "@"

    rules =
        Dict.fromList
            [ ( "beatingGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions =
                    [ EntityMatching "torch" [ HasLink "location" "player" ]
                    , EntityMatching "player" [ HasStat "strength" GT 3 ]
                    ]
                }
                , changes:
                    [
                    , SetLink "goblin" "location" "offscreen"
                    , SetLink "bagOfGold" "location" "player"
                    , IncStat "player" "strength" 1
                    ]
                , narrative: "You beat the goblin and take his gold!"
              )
            , ( "chasedAwayByGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions =
                    [ EntityMatching "torch" [ HasLink "location" "player" ]
                    ]
                }
                , changes:
                    [ SetLink "player" "location" "field"
                    ]
                , narrative: "The goblin is stronger than you, and chases you away."
              )
            , ( "tooDarkToFightGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions = []
                }
              )
              , changes: []
              , narrative: "You try to fight the goblin, but is too dark to see."
            , etc...
            ]

In this case, the `tooDarkToFightGoblin` to fight rule will be the only match if the torch is not in the inventory. Because `beatingGoblin` has more specificity than `chasedAwayByGoblin`, it will match if the player's strength is high enough, even though both rules have matching conditions.

When the player interacts with the goblin, you would call `findMatchingRule` and update your model accordingly:

    newModel =
        -- where trigger is "goblin" in this case
        findMatchingRule rules trigger model.worldModel
            |> Maybe.map
                (\( ruleID, { changes, narrative } ) ->
                    { model
                        | worldModel = WorldModel.applyChanges changes model.worldModel
                        , story = narrative
                    }
                )
            |> Maybe.withDefault model

-}

import Dict exposing (Dict)
import Narrative.WorldModel as WorldModel exposing (Query, WorldModel)


type alias EntityID =
    String


type alias RuleID =
    String


type alias Rules a =
    Dict RuleID (Rule a)


type alias Rule a =
    { a
        | trigger : EntityMatcher
        , conditions : List EntityMatcher
    }


type EntityMatcher
    = Match EntityID (List Query)
    | MatchAny (List Query)


{-| Finds the rule that matches against the provided trigger and store. If multiple rules match, this chooses the "best" match based on the most _specific_ rule. In general, the more conditions, the more specific.

In general, you would call this any time the user "interacts" with something in your game, supplying the ID of the entity that was interacted with.

While the trigger should match one of the entity IDs defined in your store, you could also programmatically call this at any time with any string, as long as there is a rule with a matching trigger. This can be useful for "abstract" events that you want to respond to, like "wait" or "next day".

-}
findMatchingRule : EntityID -> Rules a -> WorldModel b -> Maybe ( RuleID, Rule a )
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


matchTrigger : WorldModel a -> EntityID -> EntityMatcher -> Bool
matchTrigger store trigger matcher =
    case matcher of
        Match id queries ->
            (id == trigger)
                && WorldModel.assert trigger queries store

        MatchAny queries ->
            WorldModel.assert trigger queries store


matchCondition : EntityID -> WorldModel a -> EntityMatcher -> Bool
matchCondition trigger store matcher =
    case matcher of
        Match id queries ->
            queries
                |> List.map (parse trigger)
                |> (\qs ->
                        WorldModel.assert id qs store
                   )

        MatchAny queries ->
            queries
                |> List.map (parse trigger)
                |> (\qs -> WorldModel.query qs store)
                |> List.isEmpty
                |> not


parse : EntityID -> Query -> Query
parse trigger query =
    case query of
        WorldModel.HasLink key "$" ->
            WorldModel.HasLink key trigger

        _ ->
            query


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
