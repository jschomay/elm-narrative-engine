module NarrativeEngine.Core.WorldModel exposing
    ( ID, WorldModel, NarrativeComponent, Tags, Stats, Links
    , emptyTags, emptyStats, emptyLinks
    , addTag, setStat, setLink
    , tag, stat, link
    , ChangeWorld(..), ChangeEntity(..), LinkTarget(..), applyChanges
    , EntityMatcher(..), LinkMatcher(..), StatMatcher(..), Query(..), query, replaceTrigger
    , getStat, getLink
    )

{-| See how the world model is defined in the [full working example](https://github.com/jschomay/elm-narrative-engine/blob/master/src/Example.elm). Note that you can use the syntax and corresponding parsers defined in `NarrativeEngine.Utils.EntityParser` for defining entities, updates, and queries.


## Types

@docs ID, WorldModel, NarrativeComponent, Tags, Stats, Links


## Creating entities

@docs emptyTags, emptyStats, emptyLinks

These let you add tags directly to an entity.

@docs addTag, setStat, setLink

These are useful for an "entity buider pattern".

@docs tag, stat, link


## Updating entities

@docs ChangeWorld, ChangeEntity, LinkTarget, applyChanges


## Querying the world model

Queries are run against the world model to search for matching entities, or to assert that an entity has specific properties. This is useful to render a list of characters in a given location for example. The engine uses this when checking rules.

@docs EntityMatcher, LinkMatcher, StatMatcher, Query, query, replaceTrigger

You can get specific stats or links from an entity too.

@docs getStat, getLink

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


{-| Add a tag to a narrative component.
-}
addTag : String -> NarrativeComponent a -> NarrativeComponent a
addTag value entity =
    { entity | tags = Set.insert value entity.tags }


{-| Add a stat to a narrative component. A stat is a key and a numeric value on any scale you like.
-}
setStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
setStat key value entity =
    { entity | stats = Dict.insert key value entity.stats }


{-| Add a link to a narrative component. The key is the type of relationship, and the value is intended to be the id of another entity.
-}
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
-}
type ChangeEntity
    = AddTag String
    | RemoveTag String
    | SetStat String Int
    | IncStat String Int
    | DecStat String Int
    | SetLink String LinkTarget


{-| Links can set to a specific entity or you can supply an entity and key to lookup a link.

You can use `$` as the `ID` in both cases to reference the entity ID that triggered the rule (useful for generic rules).

Caution, if the look up entity or link isn't found this will keep the original link.

-}
type LinkTarget
    = SpecificLinkTarget ID
    | LookUpLinkTarget ID String


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
                    query (MatchAny queries) updated_store
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

                SetLink key (SpecificLinkTarget linkID) ->
                    update id (setLink key (parseID linkID)) updated_store

                SetLink key (LookUpLinkTarget linkID linkKey) ->
                    updated_store
                        |> Dict.get (parseID linkID)
                        |> Maybe.andThen (.links >> Dict.get linkKey)
                        |> Maybe.map (\targetID -> update id (setLink key targetID) updated_store)
                        -- noop if entity or link key not found
                        |> Maybe.withDefault updated_store
    in
    List.foldl applyUpdate store entityUpdates


{-| Semantic means for matching entities. Specifies an optional entity ID and a list of queries to match against.

Note that you can use `$` as the `ID` to reference the entity ID that triggered the rule (useful for generic rules).

-}
type EntityMatcher
    = Match ID (List Query)
    | MatchAny (List Query)


{-| Stats can either be a specific integer, or you can supply an entity ID and a stat key to do a comparison.

Note that you can use `$` as the `ID` to reference the entity ID that triggered the rule (useful in conditional narrative content).

-}
type StatMatcher
    = SpecificStat Int
    | CompareStat ID String


{-| Links can either be a specific entity matcher, or you can supply an entity ID and a link key to do a comparison.

Note that you can use `$` as the `ID` to reference the entity ID that triggered the rule (useful in conditional narrative content).

-}
type LinkMatcher
    = SpecificLink EntityMatcher
    | CompareLink ID String


{-| Semantic queries for checking properties of an entity.
-}
type Query
    = HasTag String
    | HasStat String Order StatMatcher
    | HasLink String LinkMatcher
    | Not Query


queryFn : Query -> WorldModel a -> (NarrativeComponent a -> Bool)
queryFn q store =
    case q of
        HasTag value ->
            hasTag value

        HasStat key comparator value ->
            hasStat key comparator value store

        HasLink key value ->
            hasLink key value store

        Not nestedQuery ->
            not << queryFn nestedQuery store


{-| A way to retrieve information from the store.

Provide an entity matcher to get back a list of matching entities. This is most useful for "match any" style queries, but works with specifc queries as well, just keep in mind the result is always a list.

    query (MatchAny [ HasTag "item" ]) worldModel
    -- [items...]

    query (Match "PLAYER" [ HasStat "brave" GT <| SpecificStat 5 ]) worldModel |> List.isEmpty
    -- True/False

Note that you should run `replaceTrigger` first if you have "$"'s in your matcher.

-}
query : EntityMatcher -> WorldModel a -> List ( ID, NarrativeComponent a )
query matcher store =
    case matcher of
        Match id queries ->
            findSpecific id queries store

        MatchAny queries ->
            findGeneral queries store


{-| Filters the store for entites matching the queries.
-}
findGeneral : List Query -> WorldModel a -> List ( ID, NarrativeComponent a )
findGeneral queries store =
    let
        gatherMatches id entity =
            List.all (\q -> queryFn q store entity) queries
    in
    Dict.filter gatherMatches store
        |> Dict.toList


{-| Looks up the entity and returns it if it matches the supplied queries.

Same return value as findGeneral, but better performant than filtering when you already have a specific ID.

-}
findSpecific : ID -> List Query -> WorldModel a -> List ( ID, NarrativeComponent a )
findSpecific id queries store =
    let
        matchesQueries entity =
            if List.all (\q -> queryFn q store entity) queries then
                [ ( id, entity ) ]

            else
                []
    in
    Dict.get id store
        |> Maybe.map matchesQueries
        |> Maybe.withDefault []


{-| Replaces "$" in a matcher with the supplied ID. Useful when you have a generic query and don't know the ID ahead of time.
-}
replaceTrigger : ID -> EntityMatcher -> EntityMatcher
replaceTrigger trigger matcher =
    let
        replaceInQuery q =
            case q of
                HasLink key (SpecificLink (Match "$" queries)) ->
                    HasLink key <| SpecificLink (Match trigger queries)

                HasStat key comparison (CompareStat "$" compareKey) ->
                    HasStat key comparison (CompareStat trigger compareKey)

                HasLink key (CompareLink "$" compareKey) ->
                    HasLink key (CompareLink trigger compareKey)

                _ ->
                    q

        replaceInSelector id =
            if id == "$" then
                trigger

            else
                id
    in
    case matcher of
        MatchAny queries ->
            MatchAny <| List.map replaceInQuery queries

        Match id queries ->
            Match (replaceInSelector id) <| List.map replaceInQuery queries


{-| Get a specific stat from a specific entity.
-}
getStat : ID -> String -> WorldModel a -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)


{-| Get a specific link from a specific entity.

Note, if the linked-to value doesn't exist in the world model, this will return `Nothing`.

-}
getLink : ID -> String -> WorldModel a -> Maybe ID
getLink id key store =
    Dict.get id store
        |> Maybe.andThen (.links >> Dict.get key)
        |> Maybe.andThen
            (\linkedID ->
                if Dict.member linkedID store then
                    Just linkedID

                else
                    Nothing
            )


{-| Check if a specific entity has a specific tag.
-}
hasTag : String -> NarrativeComponent a -> Bool
hasTag value entity =
    Set.member value entity.tags


{-| If the provided stat doesn't exist, it defaults to 0 (this makes it easier to query against stats that haven't been set yet, without having to preset every stat in your starting stat).
-}
hasStat : String -> Order -> StatMatcher -> WorldModel a -> NarrativeComponent a -> Bool
hasStat key comparator statMatcher store entity =
    case statMatcher of
        SpecificStat value ->
            entity.stats
                |> Dict.get key
                |> Maybe.withDefault 0
                |> (\actual -> compare actual value == comparator)

        CompareStat compareID compareKey ->
            Dict.get compareID store
                |> Maybe.andThen (.stats >> Dict.get compareKey)
                |> Maybe.map2 compare (Dict.get key entity.stats)
                |> Maybe.map ((==) comparator)
                |> Maybe.withDefault False


{-| Note, if the linked-to id doesn't exist in the world model, this will fail
-}
hasLink : String -> LinkMatcher -> WorldModel a -> NarrativeComponent a -> Bool
hasLink key linkMatcher store entity =
    let
        assertMatch matcher actualID =
            case matcher of
                Match expectedID qs ->
                    (expectedID == actualID)
                        && (findSpecific actualID qs store |> List.isEmpty |> not)

                MatchAny qs ->
                    findSpecific actualID qs store |> List.isEmpty |> not
    in
    case linkMatcher of
        SpecificLink entityMatcher ->
            entity.links
                |> Dict.get key
                |> Maybe.map (assertMatch entityMatcher)
                |> Maybe.withDefault False

        CompareLink compareID compareKey ->
            Dict.get compareID store
                |> Maybe.andThen (.links >> Dict.get compareKey)
                |> Maybe.map2 (==) (Dict.get key entity.links)
                |> Maybe.withDefault False
