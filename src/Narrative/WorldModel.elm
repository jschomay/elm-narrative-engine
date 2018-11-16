module Narrative.WorldModel exposing
    ( WorldModel, startingState
    , Entity(..), ID, entity
    , applyChanges
    , update, addTag, removeTag, setStat, incStat, decStat, setLink
    , query, assert
    , hasTag, hasStat, hasLink
    , getStat, getLink
    , ChangeWorld(..), Query(..)
    )

{-| A store that describes the story world. It includes all of the entities in the story world, along with their properties. Properties include `tags`, `stats`, and `links`.

These entities and properties are used in your rules. Specifically, these work as both "salience" and "quality/stats" based systems (described very well in Emily Shot's blog post <https://emshort.blog/2016/04/12/beyond-branching-quality-based-and-salience-based-narrative-structures/>), which can provide a more flexible story, with higher player agency, and also simplifies the story rules by allowing them to be more generic.

Note that these properties are only meant to track information specific to the narrative engine. Any other properties you need, like a name and description, or sprite dimensions, should be stored and handled in a separate system (consider using the Entity Component System pattern for this).

You need to build your world to start with, and then you can query it, and run your rules against it, and update it over time:

    worldModel =
        [ entity "player"
            |> addTag "character"
            |> setStat "strength" 5
            |> setStat "caveExplorationQuestProgress" 1
            |> setLink "location" "cave"
        , entity "goblin"
            |> addTag "character"
            |> addTag "enemy"
            |> setStat "strength" 3
            |> setLink "location" "cave"
        , entity "torch"
            |> addTag "item"
            |> setStat "illumination" 7
            |> setLink "location" "player"
        , entity "bagOfGold"
            |> addTag "item"
            |> addTag "special"
            |> setLink "location" "cave"
        , entity "cave"
            |> addTag "location"
            |> addTag "dark"
        , entity "offscreen"
            |> addTag "location"
        ]
            |> WorldModel.startingState

    currentLocation =
        getLink "player" "location"

    inventory =
        query [ HasTag "item", HasLink "location" "player" ] worldModel

    newWorldModel =
        applyChanges
            [ SetLink "bagOfGold" "location" "player"
            , incStat "player" "caveExplorationQuestProgress" 1
            , SetLink "goblin" "location" "offscreen"
            ]
            worldModel

Note that you are not restricted to the traditional "items/characters/locations" world model. You can define your entities with what ever properties you want to fit any story world.


### Creating / Updating

@docs WorldModel, startingState

@docs Entity, ID, entity

Favor using `applyChanges` with `ChangeWorld` declarations over `update` with the direct setters.

@docs ChangeWorld(..), applyChanges

@docs update, addTag, removeTag, setStat, incStat, decStat, setLink


### Querying

Queries are run against the store to assert a condition or select entities. This is useful to render a list of characters in a given location for example. The engine uses `assert` when checking rules.

@docs Query(..), query, assert

Favor using `query` over using these direct assertions.

@docs hasTag, hasStat, hasLink


### Accessing

The engine doesn't need these, but they may be useful to the view (to get the current location of the player for example `currentLocation = getLink "player" "location" store` and `health = getStat "player" "health"`.)

@docs getStat, getLink

-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias ID =
    String


type Entity
    = Entity ID EntityValues


type alias EntityValues =
    { tags : Set String
    , stats : Dict String Int
    , links : Dict String ID
    }


type alias WorldModel =
    Dict ID EntityValues


entity : ID -> Entity
entity id =
    Entity id
        { tags = Set.empty
        , stats = Dict.empty
        , links = Dict.empty
        }


{-| Add a tag to an entity. Examples: "item", "edible", "poisonous", "SmithFamily", etc.
-}
addTag : String -> Entity -> Entity
addTag tag (Entity id e) =
    { e | tags = Set.insert tag e.tags }
        |> Entity id


{-| Add a stat to an entity. A stat is a key and a numeric value on any scale you like. Examples: "health", "honor", "plotProgression", "bars of gold", etc.
-}
setStat : String -> Int -> Entity -> Entity
setStat key value (Entity id e) =
    { e | stats = Dict.insert key value e.stats }
        |> Entity id


{-| Sets a link, overriding any existing link with the same key. The value is intended to be the id of another entity, which allows queries like getting the inventory, or the current location, or characters in a location, etc.
-}
setLink : String -> ID -> Entity -> Entity
setLink key value (Entity id e) =
    { e | links = Dict.insert key value e.links }
        |> Entity id


removeTag : String -> Entity -> Entity
removeTag tag (Entity id e) =
    { e | tags = Set.remove tag e.tags }
        |> Entity id


incStat : String -> Int -> Entity -> Entity
incStat key delta (Entity id e) =
    { e | stats = Dict.update key (Maybe.map <| (+) delta) e.stats }
        |> Entity id


decStat : String -> Int -> Entity -> Entity
decStat key delta (Entity id e) =
    { e | stats = Dict.update key (Maybe.map <| \current -> current - delta) e.stats }
        |> Entity id


{-| The starting state of the world model. You need to store this in your main model.
-}
startingState : List Entity -> WorldModel
startingState entities =
    entities
        |> (List.map <| \(Entity id values) -> ( id, values ))
        |> Dict.fromList


{-| Programmatically update the store.

    newWorldModel =
        update "item1" (addTag "updated") worldModel

-}
update : ID -> (Entity -> Entity) -> WorldModel -> WorldModel
update id updateFn store =
    Dict.update id
        (Maybe.map
            (Entity id
                >> updateFn
                >> (\(Entity _ values) -> values)
            )
        )
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

    newWorldModel =
        applyChanges
            [ AddTag "item1" "extraSpecial"
            , SetLink "item1" "heldBy" "character1"
            ]
            worldModel

-}
applyChanges : List ChangeWorld -> WorldModel -> WorldModel
applyChanges changes store =
    let
        applyChange change acc =
            case change of
                AddTag id tag ->
                    update id (addTag tag) acc

                RemoveTag id tag ->
                    update id (removeTag tag) acc

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


queryFn : Query -> (ID -> WorldModel -> Bool)
queryFn q =
    case q of
        HasTag tag ->
            \id -> hasTag id tag

        HasStat key comparator value ->
            \id -> hasStat id key comparator value

        HasLink key value ->
            \id -> hasLink id key value

        Not nestedQuery ->
            \id store -> not <| queryFn nestedQuery id store


{-| A way to retrieve information from the store, based on queries. Returns a list of entity ids.

itemsInLocation = query [ HasTag "item", HasLink "placement" currentLocation] worldModel

-}
query : List Query -> WorldModel -> List ID
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
assert : ID -> List Query -> WorldModel -> Bool
assert id queries store =
    List.all (queryFn >> (\q -> q id store)) queries


getStat : ID -> String -> WorldModel -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)


getLink : ID -> String -> WorldModel -> Maybe ID
getLink id key store =
    Dict.get id store
        |> Maybe.andThen (.links >> Dict.get key)


hasTag : ID -> String -> WorldModel -> Bool
hasTag id tag store =
    Dict.get id store
        |> Maybe.map (.tags >> Set.member tag)
        |> Maybe.withDefault False


hasStat : ID -> String -> Order -> Int -> WorldModel -> Bool
hasStat id key comparator value store =
    getStat id key store
        |> Maybe.map (\actual -> compare actual value == comparator)
        |> Maybe.withDefault False


hasLink : ID -> String -> ID -> WorldModel -> Bool
hasLink id key value store =
    getLink id key store == Just value
