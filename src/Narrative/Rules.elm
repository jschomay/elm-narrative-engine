module Narrative.Rules exposing (Condition(..), EntityID, Rule, RuleID, Rules, Trigger(..), findMatchingRule)

{-| Rules are a declarative way of describing meaningful events in your story.

They are made up of two parts: a "trigger", and a set of "conditions". Each time you call `findMatchingRule`, the engine will test all of your rules against the provided trigger and the current state of your story world, and will find the best matching rule (if one exists).

Because `Rule`s are extended records, you can also include other useful data on them, such as how the state of your story world should change, or which specific narrative to show, or which sound effect to play, etc. The engine will ignore these fields, but your client can act on them when a rule matches.

@doc EntityID, RuleID, Rules, Rule, Trigger(..), Condition(..), findMatchingRule

Example rules using the example store in `Narrative.WorldModel`:

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
import Narrative.WorldModel as WorldModel exposing (WorldModel)


type alias EntityID =
    String


type alias RuleID =
    String


type alias Rules a =
    Dict RuleID (Rule a)


type alias Rule a =
    { a
        | trigger : Trigger
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
findMatchingRule : EntityID -> Rules a -> WorldModel b -> Maybe ( RuleID, Rule a )
findMatchingRule trigger rules store =
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


matchesTrigger : EntityID -> Trigger -> Bool
matchesTrigger id trigger =
    case trigger of
        TriggerMatching triggerID ->
            id == triggerID


matchesConditions : WorldModel a -> List Condition -> Bool
matchesConditions store conditions =
    List.all (matchesCondition store) conditions


matchesCondition : WorldModel a -> Condition -> Bool
matchesCondition store condition =
    case condition of
        EntityMatching entityID queries ->
            WorldModel.assert entityID queries store


weight : Rule a -> Int
weight rule =
    conditionsSpecificity rule


{-| 10 points per entity matcher, plus 1 point per query per entity
-}
conditionsSpecificity : Rule a -> Int
conditionsSpecificity { conditions } =
    conditions
        |> List.foldl
            (\(EntityMatching _ queries) acc ->
                acc + 10 + List.length queries
            )
            0
