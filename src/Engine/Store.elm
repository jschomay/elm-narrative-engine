module Engine.Store exposing
    ( Store, initStore
    , Entity(..), ID, entity, addTag, setProperty, setStat
    , getProperty, getStat
    , Query(..), query, hasTag, hasProperty, hasStat
    )

{-| A store to hold all of the entities in the story. Each entity is an id associated with the properites of an Entity, namely, tags, properites, links, and stats, which can be queried against, fetched, and set.

You can use your own store instead of this one if your game already has a store (like a scene graph for example), as long as it can conform to interface to the engine.


### Creating / Updating

@docs Store, initStore

@docs Entity, ID, entity, addTag, removeTag, setProperty, setStat, incStat, decStat, setLink


### Querying

@docs getProperty, getStat, getLink

@docs Query, query, hasTag, hasProperty, hasStat, hasLink

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



-- TODO
-- setLink


initStore : List Entity -> Store
initStore entities =
    entities
        |> (List.map <| \(Entity id values) -> ( id, values ))
        |> Dict.fromList


query : List Query -> Store -> List Entity
query queries store =
    -- TODO
    []



-- TODO
-- removeTag
-- incStat : String -> Int -> Entity -> Entity
-- decStat : String -> Int -> Entity -> Entity


getProperty : ID -> String -> Store -> Maybe String
getProperty id key store =
    Dict.get id store
        |> Maybe.andThen (.properties >> Dict.get key)


getStat : ID -> String -> Store -> Maybe Int
getStat id key store =
    Dict.get id store
        |> Maybe.andThen (.stats >> Dict.get key)



-- TODO
-- getLink : ID -> String -> Store -> Maybe String


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



-- TODO
-- hasLink : ID -> String -> ID -> Store -> Bool
