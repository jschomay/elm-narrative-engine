module Narrative.WorldModel exposing
    ( WorldModel, NarrativeComponent, ID, Tags, Stats, Links
    , emptyTags, emptyStats, emptyLinks, tag, stat, link
    , ChangeWorld(..), applyChanges
    , Query(..), query, assert
    , getStat, getLink
    )

{-| Your story/game will have a store, or "world model", of all of the entities that live in your world, and the various properties that describe them. The narrative engine uses your store, but doesn't define it or own it. This means that it has to make some assumptions about it.

@docs WorldModel, NarrativeComponent, ID, Tags, Stats, Links

The entity ids and the `tags,`stats`, and`links`properties are used by`Narrative.Rules\`. Specifically, these work as both "salience-based" and "quality/stats-based" systems (described very well in [Emily Shot's blog post](https://emshort.blog/2016/04/12/beyond-branching-quality-based-and-salience-based-narrative-structures/)), which can provide a more flexible story, with higher player agency, and also simplifies the story rules by allowing them to be more generic.

Note that these properties are only meant to track information specific to the narrative engine. Any other properties you need, like a name and description, or sprite dimensions, should be stored and handled in a separate system (consider using the Entity Component System pattern for this).


### Example usage:

    -- another component that your entities use
    type alias DescriptionComponent a =
        { a | name : String, description : String }


    -- your game entity type, defined in terms of the components it uses
    type alias MyEntity =
        NarrativeComponent (DescriptionComponent {})

    type alias MyWorldModel =
        Dict String MyEntity

    emptyEntity : MyEntity
    emptyEntity =
        { name = ""
        , description = ""
        , tags = emptyTags
        , stats = emptyStats
        , links = emptyLinks
        }


    -- a simple helper function for fluently describing your initial world model
    entity : String -> ( String, MyEntity )
    entity id =
        ( id, emptyEntity )


    -- describing your world model (this only shows the narrative component properties, but you could add properties for other components in a very similar way)
    worldModel : MyWorldModel
    worldModel =
        Dict.fromList
            [ entity "player"
                |> tag "character"
                |> stat "strength" 5
                |> stat "caveExplorationQuestProgress" 1
                |> link "location" "cave"
            , entity "goblin"
                |> tag "character"
                |> tag "enemy"
                |> stat "strength" 3
                |> link "location" "cave"
            , entity "torch"
                |> tag "item"
                |> stat "illumination" 7
                |> link "location" "player"
            , entity "bagOfGold"
                |> tag "item"
                |> tag "special"
                |> link "location" "cave"
            , entity "cave"
                |> tag "location"
                |> tag "dark"
            , entity "offscreen"
                |> tag "location"
                |> tag "invisible"
            ]


    -- some example queries:
    currentLocation =
        getLink "player" "location" worldModel

    inventory =
        query [ HasTag "item", HasLink "location" "player" ] worldModel


    -- updating the world model
    newWorldModel =
        applyChanges
            [ SetLink "bagOfGold" "location" "player"
            , incStat "player" "caveExplorationQuestProgress" 1
            , SetLink "goblin" "location" "offscreen"
            ]
            worldModel

Note that you are not restricted to the traditional "items/characters/locations" world model. You can define your entities with what ever properties you want, to fit any story world.


### Full API

Setting up the starting state of your entities:

@docs emptyTags, emptyStats, emptyLinks, tag, stat, link

Updating your entities:

@docs ChangeWorld, applyChanges


### Querying

Queries are run against the store to assert a condition or select particular entities. This is useful to render a list of characters in a given location for example. The engine uses `assert` when checking rules.

@docs Query, query, assert

You can directly access certain property values, to get the current location, or player's health for example.

@docs getStat, getLink

-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias ID =
    String


{-| This is what the engine thinks the world model of all of the entities in your store/game looks like. It must be a `Dict` of "entities" with keys representing entity ids as `String`s, and values of `NarrativeComponent a`'s. As long as it looks like this, the engine can operate on it as it needs.
-}
type alias WorldModel a =
    Dict ID (NarrativeComponent a)


{-| This is what the engine thinks your entities look like. As long as they are a record that includes the `tags`, `stats`, and `links` fields, the engine can operate on them. You should avoid using or changing these fields directly, and use the API in this module instead.

Because this is an extensible record, you can have other properties on your entities as well (like "name" and "description" or "sprite" for example), which works well with the "Entity Component System" pattern.

-}
type alias NarrativeComponent a =
    { a
        | tags : Tags
        , stats : Stats
        , links : Links
    }


type alias Tags =
    Set String


type alias Stats =
    Dict String Int


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


{-| A helper function to add a tag to an entity when setting up your world model. Examples: "item", "edible", "poisonous", "SmithFamily", etc.
-}
tag : String -> ( ID, NarrativeComponent a ) -> ( ID, NarrativeComponent a )
tag value ( id, entity ) =
    ( id, addTag value entity )


{-| A helper function to add a stat to an entity when setting up your world model. A stat is a key and a numeric value on any scale you like. Examples: "health", "honor", "plotProgression", "bars of gold", etc.
-}
stat : String -> Int -> ( ID, NarrativeComponent a ) -> ( ID, NarrativeComponent a )
stat key value ( id, entity ) =
    ( id, setStat key value entity )


{-| A helper function to add a link to an entity when setting up your world model. The key is the type of relationship, and the value is intended to be the id of another entity, which allows queries like getting the inventory, or the current location, or characters in a location, etc.
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


incStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
incStat key delta entity =
    { entity | stats = Dict.update key (Maybe.map <| (+) delta) entity.stats }


decStat : String -> Int -> NarrativeComponent a -> NarrativeComponent a
decStat key delta entity =
    { entity | stats = Dict.update key (Maybe.map <| \current -> current - delta) entity.stats }


update : ID -> (NarrativeComponent a -> NarrativeComponent a) -> WorldModel a -> WorldModel a
update id updateFn store =
    Dict.update id
        (Maybe.map updateFn)
        store


{-| Declarative descriptions of how the world should change, designed to be used with rules.
-}
type ChangeWorld
    = AddTag ID String
    | RemoveTag ID String
    | SetStat ID String Int
    | IncStat ID String Int
    | DecStat ID String Int
    | SetLink ID String ID


{-| Update the store based on ChangeWorld declarations.
-}
applyChanges : List ChangeWorld -> WorldModel a -> WorldModel a
applyChanges changes store =
    let
        applyChange change acc =
            case change of
                AddTag id value ->
                    update id (addTag value) acc

                RemoveTag id value ->
                    update id (removeTag value) acc

                SetStat id key value ->
                    update id (setStat key value) acc

                IncStat id key amount ->
                    update id (incStat key amount) acc

                DecStat id key amount ->
                    update id (decStat key amount) acc

                SetLink id key value ->
                    update id (setLink key value) acc
    in
    List.foldl applyChange store changes


type Query
    = HasTag String
    | HasStat String Order Int
    | HasLink String ID
    | Not Query


queryFn : Query -> (ID -> WorldModel a -> Bool)
queryFn q =
    case q of
        HasTag valud ->
            \id -> hasTag id valud

        HasStat key comparator value ->
            \id -> hasStat id key comparator value

        HasLink key value ->
            \id -> hasLink id key value

        Not nestedQuery ->
            \id store -> not <| queryFn nestedQuery id store


{-| A way to retrieve information from the store. Returns a list of entity ids.
-}
query : List Query -> WorldModel a -> List ID
query queries store =
    let
        gatherMatches id _ matches =
            if assert id queries store then
                id :: matches

            else
                matches
    in
    Dict.foldl gatherMatches [] store


{-| Asserts if the current state of the store matches the given queries. Used by `Narrative.Rules.findMatchingRule`. You can also use it for your own custom logic if needed.
-}
assert : ID -> List Query -> WorldModel a -> Bool
assert id queries store =
    List.all (queryFn >> (\q -> q id store)) queries


getStat : ID -> String -> WorldModel a -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)


getLink : ID -> String -> WorldModel a -> Maybe ID
getLink id key store =
    Dict.get id store
        |> Maybe.andThen (.links >> Dict.get key)


hasTag : ID -> String -> WorldModel a -> Bool
hasTag id value store =
    Dict.get id store
        |> Maybe.map (.tags >> Set.member value)
        |> Maybe.withDefault False


hasStat : ID -> String -> Order -> Int -> WorldModel a -> Bool
hasStat id key comparator value store =
    getStat id key store
        |> Maybe.map (\actual -> compare actual value == comparator)
        |> Maybe.withDefault False


hasLink : ID -> String -> ID -> WorldModel a -> Bool
hasLink id key value store =
    getLink id key store == Just value