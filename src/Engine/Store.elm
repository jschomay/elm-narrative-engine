module Engine.Store exposing
    ( Store, initStore, update
    , Entity(..), ID, entity, addTag, removeTag, setProperty, setStat, incStat, decStat, setLink
    , Query(..), query, hasTag, hasProperty, hasStat, hasLink
    , getProperty, getStat, getLink
    )

{-| A store to hold all of the entities in the story. Each entity is an id associated with the properites of an Entity, namely, tags, properites, links, and stats, which can be queried against, fetched, and set.

You can use your own store instead of this one if your game already has a store (like a scene graph for example), as long as it provides an interface for the engine to "plug into."


### Creating / Updating

@docs Store, initStore, update

@docs Entity, ID, entity, addTag, removeTag, setProperty, setStat, incStat, decStat, setLink


### Querying

Queries are run against the store to return a list of Entities matching that query. This is useful to render a list of characters in a given location for example. The engine also uses these "matchers" to find matching rules.

@docs Query, query, hasTag, hasProperty, hasStat, hasLink


### Accessing

The engine doesn't need these, but they may be useful to the view (to get the current location of the player for example `currentLocation = getLink "playerId" "location" store`.)

Note that these should only be used for properties controlled by tne engine. For example, storing and getting a name and description, or sprite dimentions, etc should be handled in a separate system (consider using the Entity Componsent System pattern for this).

@docs getProperty, getStat, getLink

-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias ID =
    String


type Entity
    = Entity ID EntityValues


type alias EntityValues =
    { tags : Set String
    , properties : Dict String String
    , stats : Dict String Int
    , links : Dict String ID
    }


type alias Store =
    Dict ID EntityValues


type Query
    = Tag String
    | Property String String
    | Stat String Int
    | Link String ID


entity : ID -> Entity
entity id =
    Entity id
        { tags = Set.empty
        , properties = Dict.empty
        , stats = Dict.empty
        , links = Dict.empty
        }


addTag : String -> Entity -> Entity
addTag tag (Entity id e) =
    { e | tags = Set.insert tag e.tags }
        |> Entity id


setProperty : String -> String -> Entity -> Entity
setProperty key value (Entity id e) =
    { e | properties = Dict.insert key value e.properties }
        |> Entity id


setStat : String -> Int -> Entity -> Entity
setStat key value (Entity id e) =
    { e | stats = Dict.insert key value e.stats }
        |> Entity id


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


initStore : List Entity -> Store
initStore entities =
    entities
        |> (List.map <| \(Entity id values) -> ( id, values ))
        |> Dict.fromList


update : ID -> (Entity -> Entity) -> Store -> Store
update id updateFn store =
    Dict.update id
        (Maybe.map
            (Entity id
                >> updateFn
                >> (\(Entity _ values) -> values)
            )
        )
        store


query : List Query -> Store -> List Entity
query queries store =
    -- TODO
    []


getProperty : ID -> String -> Store -> Maybe String
getProperty id key store =
    Dict.get id store
        |> Maybe.andThen (.properties >> Dict.get key)


getStat : ID -> String -> Store -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)


getLink : ID -> String -> Store -> Maybe ID
getLink id key store =
    Dict.get id store
        |> Maybe.andThen (.links >> Dict.get key)


hasTag : ID -> String -> Store -> Bool
hasTag id tag store =
    Dict.get id store
        |> Maybe.map (.tags >> Set.member tag)
        |> Maybe.withDefault False


hasProperty : ID -> String -> String -> Store -> Bool
hasProperty id key value store =
    getProperty id key store == Just value


hasStat : ID -> String -> Order -> Int -> Store -> Bool
hasStat id key comparator value store =
    getStat id key store
        |> Maybe.map (\actual -> compare actual value == comparator)
        |> Maybe.withDefault False


hasLink : ID -> String -> ID -> Store -> Bool
hasLink id key value store =
    getLink id key store == Just value
