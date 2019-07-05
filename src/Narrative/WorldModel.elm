module Narrative.WorldModel exposing
    ( ID, WorldModel, NarrativeComponent, Tags, Stats, Links
    , emptyTags, emptyStats, emptyLinks, tag, stat, link
    , ChangeWorld(..), ChangeEntity(..), applyChanges
    , EntityMatcher(..), Query(..), query, assert, assertMatch, getStat, getLink
    )

{-| See how the world model is defined in the [full working example](https://github.com/jschomay/elm-narrative-engine/blob/master/src/Example.elm).


## Types

@docs ID, WorldModel, NarrativeComponent, Tags, Stats, Links


## Creating entities

@docs emptyTags, emptyStats, emptyLinks, tag, stat, link


## Updating entities

@docs ChangeWorld, ChangeEntity, applyChanges


## Querying the world model

Queries are run against the world model to assert a condition or select particular entities. This is useful to render a list of characters in a given location for example. The engine uses these when checking rules.

@docs EntityMatcher, Query, query, assert, assertMatch, getStat, getLink

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| A unique identifier for each entity.
-}
type alias ID =
    String


{-| Your story/game will have a store, or "world model", of all of the entities that live in your world, and the various properties that describe them. The narrative engine expects your world model to have this shape.
-}
type alias WorldModel a =
    Dict ID (NarrativeComponent a)


{-| Entities are just IDs coupled with various fields of information. The engine requires that your entities have `tags`, `stats`, and `links` fields.

It uses these fields to track "salience-based" and "quality/stats-based" narratives (described very well in [Emily Shot's blog post](https://emshort.blog/2016/04/12/beyond-branching-quality-based-and-salience-based-narrative-structures/)), which can provide a more flexible and robust story.

Because this is an extensible record, you can have other properties on your entities as well (like "name" and "description" or "sprite" for example), which works well with the "Entity Component System" design pattern.

Note that you are not restricted to the traditional "items/characters/locations" world model. You can define your entities with what ever properties you want, to fit any story world.

-}
type alias NarrativeComponent a =
    { a
        | tags : Tags
        , stats : Stats
        , links : Links
    }


{-| "Tags" on an entity.

Examples: "item", "edible", "poisonous", "SmithFamily", etc.

-}
type alias Tags =
    Set String


{-| "Stats" on an entity.

Examples: "health", "honor", "plotProgression", "bars of gold", etc.

-}
type alias Stats =
    Dict String Int


{-| "Links" on an entity.

Examples: "locatedIn", "knows", "suspects", etc.

-}
type alias Links =
    Dict String ID


{-| A empty starting state for the "tags" property
-}
emptyTags : Tags
emptyTags =
    Set.empty


{-| A empty starting state for the "stats" property
-}
emptyStats : Stats
emptyStats =
    Dict.empty


{-| A empty starting state for the "links" property
-}
emptyLinks : Links
emptyLinks =
    Dict.empty


{-| A helper function to add a tag to an entity when setting up your world model.
-}
tag : String -> ( ID, NarrativeComponent a ) -> ( ID, NarrativeComponent a )
tag value ( id, entity ) =
    ( id, addTag value entity )


{-| A helper function to add a stat to an entity when setting up your world model. A stat is a key and a numeric value on any scale you like.
-}
stat : String -> Int -> ( ID, NarrativeComponent a ) -> ( ID, NarrativeComponent a )
stat key value ( id, entity ) =
    ( id, setStat key value entity )


{-| A helper function to add a link to an entity when setting up your world model. The key is the type of relationship, and the value is intended to be the id of another entity.
-}
link : String -> ID -> ( ID, NarrativeComponent a ) -> ( ID, NarrativeComponent a )
link key value ( id, entity ) =
    ( id, setLink key value entity )


addTag : String -> NarrativeComponent a -> NarrativeComponent a
addTag value entity =
    { entity | tags = Set.insert value entity.tags }


setStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
setStat key value entity =
    { entity | stats = Dict.insert key value entity.stats }


setLink : String -> ID -> NarrativeComponent a -> NarrativeComponent a
setLink key value entity =
    { entity | links = Dict.insert key value entity.links }


removeTag : String -> NarrativeComponent a -> NarrativeComponent a
removeTag value entity =
    { entity | tags = Set.remove value entity.tags }


{-| If the specified stat has not been set, this will assume it was 0, and adjust from there.
-}
incStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
incStat key delta entity =
    let
        current =
            Dict.get key entity.stats |> Maybe.withDefault 0
    in
    { entity | stats = Dict.insert key (current + delta) entity.stats }


{-| If the specified stat has not been set, this will assume it was 0, and adjust from there.
-}
decStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
decStat key delta entity =
    let
        current =
            Dict.get key entity.stats |> Maybe.withDefault 0
    in
    { entity | stats = Dict.insert key (current - delta) entity.stats }


update : ID -> (NarrativeComponent a -> NarrativeComponent a) -> WorldModel a -> WorldModel a
update id updateFn store =
    Dict.update id
        (Maybe.map updateFn)
        store


{-| Declarative statements of how an entity should change, designed to be used with rules.

Note that you can use `$` as the `ID` to reference the entity ID that triggered the rule (useful for generic rules).

-}
type ChangeWorld
    = Update ID (List ChangeEntity)
    | UpdateAll (List Query) (List ChangeEntity)


{-| Declarative statements for changing a property on an entity.

Note that you can use `$` as the `ID` in `SetLink` to reference the entity ID that triggered the rule (useful for generic rules).

-}
type ChangeEntity
    = AddTag String
    | RemoveTag String
    | SetStat String Int
    | IncStat String Int
    | DecStat String Int
    | SetLink String ID


{-| Update the world model based on a list of changes. Also takes the id of the entity that triggered the rule to allow changes to use trigger matching (with `$`).
-}
applyChanges : List ChangeWorld -> ID -> WorldModel a -> WorldModel a
applyChanges entityUpdates trigger store =
    let
        parseID id =
            case id of
                "$" ->
                    trigger

                _ ->
                    id

        updateEntity id =
            List.foldl (applyChange id)

        applyUpdate entityUpdate updated_store =
            case entityUpdate of
                Update id changes ->
                    updateEntity (parseID id) updated_store changes

                UpdateAll queries changes ->
                    query queries updated_store
                        -- apply all changes to all queried entities
                        |> List.foldl (\( id, _ ) acc -> updateEntity id acc changes) updated_store

        applyChange id change updated_store =
            case change of
                AddTag tag_ ->
                    update id (addTag tag_) updated_store

                RemoveTag tag_ ->
                    update id (removeTag tag_) updated_store

                SetStat key stat_ ->
                    update id (setStat key stat_) updated_store

                IncStat key amount ->
                    update id (incStat key amount) updated_store

                DecStat key amount ->
                    update id (decStat key amount) updated_store

                SetLink key linkID ->
                    update id (setLink key (parseID linkID)) updated_store
    in
    List.foldl applyUpdate store entityUpdates


{-| Semantic means for matching entities. Specifies an optional entity ID and a list of queries to match against.

Note that you can use `$` as the `ID` to reference the entity ID that triggered the rule (useful for generic rules).

-}
type EntityMatcher
    = Match ID (List Query)
    | MatchAny (List Query)


{-| Semantic queries for checking properties of an entity.
-}
type Query
    = HasTag String
    | HasStat String Order Int
    | HasLink String EntityMatcher
    | Not Query


queryFn : Query -> WorldModel a -> (NarrativeComponent a -> Bool)
queryFn q store =
    case q of
        HasTag value ->
            hasTag value

        HasStat key comparator value ->
            hasStat key comparator value

        HasLink key value ->
            hasLink key value store

        Not nestedQuery ->
            not << queryFn nestedQuery store


{-| A way to retrieve information from the store.
-}
query : List Query -> WorldModel a -> List ( ID, NarrativeComponent a )
query queries store =
    let
        gatherMatches id entity =
            List.all (\q -> queryFn q store entity) queries
    in
    Dict.filter gatherMatches store
        |> Dict.toList


{-| Asserts if the current state of the world model matches the given queries.
-}
assert : ID -> List Query -> WorldModel a -> Bool
assert id queries store =
    let
        maybeEntity =
            Dict.get id store

        matchesQueries entity =
            List.all (\q -> queryFn q store entity) queries
    in
    maybeEntity
        |> Maybe.map matchesQueries
        |> Maybe.withDefault False


{-| Get a specific stat from a specific entity.
-}
getStat : ID -> String -> WorldModel a -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)


{-| Get a specific link from a specific entity.
-}
getLink : ID -> String -> WorldModel a -> Maybe ID
getLink id key store =
    Dict.get id store
        |> Maybe.andThen (.links >> Dict.get key)


{-| Check if a specific entity has a specific tag.
-}
hasTag : String -> NarrativeComponent a -> Bool
hasTag value entity =
    Set.member value entity.tags


{-| If the provided stat doesn't exist, it defaults to 0 (this makes it easier to query against stats that haven't been set yet, without having to preset every stat in your starting stat).
-}
hasStat : String -> Order -> Int -> NarrativeComponent a -> Bool
hasStat key comparator value entity =
    entity.stats
        |> Dict.get key
        |> Maybe.withDefault 0
        |> (\actual -> compare actual value == comparator)


hasLink : String -> EntityMatcher -> WorldModel a -> NarrativeComponent a -> Bool
hasLink key matcher store entity =
    entity.links
        |> Dict.get key
        |> Maybe.map (assertMatch matcher store)
        |> Maybe.withDefault False


{-| A utility to test an `EntityMatcher` matcher against a specific entity `ID`.
-}
assertMatch : EntityMatcher -> WorldModel a -> ID -> Bool
assertMatch matcher store expectedID =
    case matcher of
        Match id qs ->
            (id == expectedID)
                && assert id qs store

        MatchAny qs ->
            assert expectedID qs store
