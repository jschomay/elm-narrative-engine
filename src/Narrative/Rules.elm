module Narrative.Rules exposing (Condition(..), EntityID, Rule, RuleID, Rules, Trigger(..), findMatchingRule)

{-| Rules are a declarative way of describing meaningful events in your story.

They are made up of two parts: a "trigger", and a set of "conditions". Each time you call `findMatchingRule`, the engine will test all of your rules against the provided trigger and the current state of your story world, and will return the id of a matching rule (if one exists). From there, you can carry out any side-effects specific to that rule id, like changing the state of your story world, or showing a specific narrative, or playing a sound effect, etc.

@doc EntityID, RuleID, Rules, Rule, Trigger(..), Condition(..), findMatchingRule

Example rules using the example store in `Narrative.WorldModel`:

    fightGoblinRules =
        Dict.fromList
            [ ( "beatingGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions =
                    [ EntityMatching "torch" [ HasLink "location" "player" ]
                    , EntityMatching "player" [ HasStat "strength" GT 3 ]
                    ]
                }
              )
            , ( "chasedAwayByGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions =
                    [ EntityMatching "torch" [ HasLink "location" "player" ]
                    ]
                }
              )
            , ( "tooDarkToFightGoblin"
              , { trigger = TriggerMatching "goblin"
                , conditions = []
                }
              )
            ]

In this case, the `tooDarkToFightGoblin` to fight rule will be the only match if the torch is not in the inventory. Because `beatingGoblin` has more specificity than `chasedAwayByGoblin`, it will match if the player's strength is high enough, even though both rules have matching conditions.

When the player interacts with the goblin, you would call:

    # where trigger in this case is "goblin"
    maybeMatchedRuleId trigger =
        findMatchingRule fightGoblinRules trigger worldModel

In your game, you would then associate one of the matched rule ids with "side effects," like a narrative to display, and a set of `ChangeWorld` declarations to apply to the store.

-}

import Dict exposing (Dict)
import Narrative.WorldModel as WorldModel exposing (WorldModel)


type alias EntityID =
    String


type alias RuleID =
    String


type alias Rules =
    Dict RuleID Rule


type alias Rule =
    { trigger : Trigger
    , conditions : List Condition
    }


type Trigger
    = TriggerMatching EntityID


type Condition
    = EntityMatching EntityID (List WorldModel.Query)


{-| Finds the rule that matches against the provided trigger and store. If multiple rules match, this chooses the "best" match based on the most _specific_ rule. In general, the more conditions, the more specific.

In general, you would call this any time the user "interacts" with something in your game, supplying the ID of the entity that was interacted with.

While the trigger should match one of the entity IDs defined in your store, you could also programmatically call this at any time with any string, as long as there is a rule with a matching trigger. This can be useful for "abstract" events that you want to respond to, like "wait" or "next day".

-}
findMatchingRule : Rules -> EntityID -> WorldModel -> Maybe RuleID
findMatchingRule rules trigger store =
    rules
        |> Dict.filter
            (\ruleId rule ->
                matchesTrigger trigger rule.trigger
                    && matchesConditions store rule.conditions
            )
        |> Dict.toList
        |> List.sortBy (Tuple.second >> weight)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first


matchesTrigger : EntityID -> Trigger -> Bool
matchesTrigger id trigger =
    case trigger of
        TriggerMatching triggerID ->
            id == triggerID


matchesConditions : WorldModel -> List Condition -> Bool
matchesConditions store conditions =
    List.all (matchesCondition store) conditions


matchesCondition : WorldModel -> Condition -> Bool
matchesCondition store condition =
    case condition of
        EntityMatching entityID queries ->
            WorldModel.assert entityID queries store


weight : Rule -> Int
weight rule =
    conditionsSpecificity rule


{-| 10 points per entity matcher, plus 1 point per query per entity
-}
conditionsSpecificity : Rule -> Int
conditionsSpecificity { conditions } =
    conditions
        |> List.foldl
            (\(EntityMatching _ queries) acc ->
                acc + 10 + List.length queries
            )
            0
