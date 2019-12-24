module Example exposing (main)

{-| This file shows a bare-bones implementation of an Elm Narrative Engine game.
-}

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import NarrativeEngine.Core.Rules as Rules
import NarrativeEngine.Core.WorldModel as WorldModel



-- First we need to set up some types.  We will define our entities.  Entities are
-- set up as extensible records, so lets add a few extra fields, such as "name" and
-- "description".  Our code will be able to read these values off of our entities and
-- do something reasonable with it (this is the Entity Component System, or ECS
-- design pattern).


{-| A "component" that adds a name and description to an entity.
-}
type alias DescriptionComponent a =
    { a | name : String, description : String }


{-| Our concrete entity, complete with our custom component (you could add additional
components in this same way if desired.
-}
type alias MyEntity =
    WorldModel.NarrativeComponent (DescriptionComponent {})


{-| Our concrete world model using our custom entities.
-}
type alias MyWorldModel =
    Dict String MyEntity


{-| A handy entity builder.
-}
entity : WorldModel.ID -> String -> String -> ( String, MyEntity )
entity id name description =
    ( id
    , { name = name
      , description = description
      , tags = WorldModel.emptyTags
      , stats = WorldModel.emptyStats
      , links = WorldModel.emptyLinks
      }
    )


{-| Populating the world model with all of our entities and their starting
properties. I am capitalizing entity IDs just as a convention, it isn't required.
Also note that we are using characters/items/locations because it is convenient for
this example, but there is nothing requiring us to use those entity types.

Note that the world model is stateful and needs to be stored in the model.

-}
initialWorldModel : MyWorldModel
initialWorldModel =
    Dict.fromList
        [ -- locations
          entity "CAVE"
            "Dark cavern"
            "Caves are dark and scary, and habitats for two things: goblins and bags of gold."
            |> WorldModel.tag "location"
            |> WorldModel.tag "dark"
        , entity "CAVE_ENTRANCE"
            "Entrance to a cave"
            "This seems like a relatively safe place to be."
            |> WorldModel.tag "location"

        -- characters
        , entity "PLAYER"
            "Yourself"
            "You are a mostly fearless explorer in search of gold."
            |> WorldModel.stat "fear" 1
            |> WorldModel.stat "bagsOfGoldCollected" 0
            |> WorldModel.link "location" "CAVE_ENTRANCE"
        , entity "GOBLIN"
            "Sleepy goblin"
            "Big, green, ugly, and can't seem to keep his eyes open."
            |> WorldModel.tag "character"
            |> WorldModel.tag "sleeping"
            |> WorldModel.link "location" "CAVE"

        -- items
        , entity "LIGHTER"
            "Pocket lighter"
            "You don't smoke, but it's useful to have a lighter on hand, though it's not much of a light source."
            |> WorldModel.tag "item"
            |> WorldModel.stat "illumination" 2
            |> WorldModel.stat "fuel" 10
            |> WorldModel.link "location" "PLAYER"
        , entity "TORCH"
            "Torch"
            "This is the go-to illumination solution for adventurers."
            |> WorldModel.tag "item"
            |> WorldModel.stat "illumination" 0
            |> WorldModel.link "location" "CAVE_ENTRANCE"
        , entity "BAG_OF_GOLD"
            "Bag of gold"
            "This is what makes it all worthwhile."
            |> WorldModel.tag "item"
            |> WorldModel.link "location" "CAVE"
        ]



-- Time to define our rules


{-| Just like entities, rules follow the ECS pattern, so we can add other fields to
them. In this case we just add a story text, but you could add more, such as a sound
effect or image path. These fields will be handled by our own code, so we could do
all kinds of fancy things, like parsing the narrative for repeated or conditional
text for example. In this case, we just keep it a simple string.
-}
type alias MyRule =
    Rules.Rule
        { narrative : String
        }


{-| Our concrete "rule book" type.
-}
type alias Rules =
    Dict String MyRule


{-| A simple rule builder helper.
-}
rule : Rules.RuleID -> MyRule -> ( Rules.RuleID, MyRule )
rule ruleID rule_ =
    ( ruleID, rule_ )


{-| All the rules that govern our story (you might think of it as our "rulebook").
These have no state, so they do not need to be stored in our model
-}
rules =
    Dict.fromList
        [ -- Here are some specific rules, meaning that they specify the entities
          -- that they apply to directly by ID
          --
          -- This one lets you enter the cave if you have enough light. (There is a
          -- "generic rule" below that will trigger if you don't have enough light.)
          -- Note how it sets a flag of "explored" on the cave, to ensure this rule
          -- matches only once.
          -- Also, note how this rule has conditions on two entities, and updates two
          -- entities.
          rule "entering the cave"
            { trigger = WorldModel.Match "CAVE" []
            , conditions =
                [ WorldModel.Match "TORCH"
                    [ WorldModel.HasLink "location" (WorldModel.Match "PLAYER" [])
                    , WorldModel.HasStat "illumination" GT 5
                    ]
                , WorldModel.Match "CAVE" [ WorldModel.Not (WorldModel.HasTag "explored") ]
                ]
            , changes =
                [ WorldModel.Update "PLAYER"
                    [ WorldModel.IncStat "fear" 2
                    , WorldModel.SetLink "location" "CAVE"
                    ]
                , WorldModel.Update "CAVE" [ WorldModel.AddTag "explored" ]
                ]
            , narrative = "You can see a short way into the cave, and bravely enter.  You hear an awful snoring sound..."
            }

        -- This one give you hint when you check the unlit torch in inventory
        , rule "observe unlit torch"
            { trigger =
                WorldModel.Match "TORCH"
                    [ WorldModel.HasLink "location" (WorldModel.Match "PLAYER" [])
                    , WorldModel.HasStat "illumination" EQ 0
                    ]
            , conditions = []
            , changes =
                []
            , narrative = "A perfectly useful torch, except for the fact that is unlit."
            }

        -- This one lights the torch when unlit torch is in inventory and the fuel of lighter is not ran out.
        , rule "light the torch"
            { trigger =
                WorldModel.Match "LIGHTER"
                    [ WorldModel.HasLink "location" (WorldModel.Match "PLAYER" [])
                    , WorldModel.HasStat "fuel" GT 0
                    ]
            , conditions =
                [ WorldModel.Match "TORCH"
                    [ WorldModel.HasLink "location" (WorldModel.Match "PLAYER" [])
                    , WorldModel.HasStat "illumination" EQ 0
                    ]
                ]
            , changes =
                [ WorldModel.Update "TORCH"
                    [ WorldModel.SetStat "illumination" 7 ]
                , WorldModel.Update "LIGHTER"
                    [ WorldModel.DecStat "fuel" 1 ]
                , WorldModel.Update "PLAYER" [ WorldModel.DecStat "fear" 1 ]
                ]
            , narrative = "Brilliant! You have a bright torch now, fear no more!"
            }

        -- This one puts you back outside if you mess with the goblin while he is
        -- sleeping
        , rule "waking the goblin"
            { trigger = WorldModel.Match "GOBLIN" [ WorldModel.HasTag "sleeping" ]
            , conditions = []
            , changes =
                [ WorldModel.Update "PLAYER"
                    [ WorldModel.SetLink "location" "CAVE_ENTRANCE"
                    , WorldModel.IncStat "fear" 5
                    ]
                ]
            , narrative = "There's an old saying, \"Let sleeping dogs lie.\"  That applies double when it comes to goblins.  Too late... the goblin wakes up and chases you out of the cave."
            }

        -- This one lets you take the torch and win
        , rule "getting the gold"
            { trigger = WorldModel.Match "BAG_OF_GOLD" [ WorldModel.HasLink "location" (WorldModel.Match "CAVE" []) ]
            , conditions = []
            , changes =
                [ WorldModel.Update "PLAYER" [ WorldModel.IncStat "bagsOfGoldCollected" 1 ]
                , WorldModel.Update "BAG_OF_GOLD" [ WorldModel.SetLink "location" "PLAYER" ]
                ]
            , narrative = "You nimbly sneak around the sleeping goblin and snatch the bag of gold!"
            }

        -- Now for some general rules.  Note that these will only be selected if a
        -- more specific rule does not match.
        --
        -- This one lets you move to a new location. It uses "MatchAny", which is a
        -- "generic matcher," because it will apply to any entity the player clicks, as
        -- long as it has the "location" tag.
        --
        -- Also Notice the "$" in the "SetLink".  This is a special character that
        -- will get replaced with the entity ID of the entity that triggered this
        -- rule (since we don't know what it is at declaration time).
        , rule
            "moving around"
            { trigger = WorldModel.MatchAny [ WorldModel.HasTag "location" ]
            , conditions = []
            , changes = [ WorldModel.Update "PLAYER" [ WorldModel.SetLink "location" "$" ] ]
            , narrative = "You go explore over there."
            }

        -- Here's one that adds any item to the player's inventory (if it isn't
        -- already there).
        -- Note how we use "$" again, this time with "Update".
        , rule "picking up items"
            { trigger =
                WorldModel.MatchAny
                    [ WorldModel.HasTag "item"
                    , WorldModel.Not (WorldModel.HasLink "location" (WorldModel.Match "PLAYER" []))
                    ]
            , conditions = []
            , changes = [ WorldModel.Update "$" [ WorldModel.SetLink "location" "PLAYER" ] ]
            , narrative = "That might be useful.  You take it with you."
            }

        -- Another generic rule about entering dark places.  It is only slightly more
        -- specific than the rule for moving around, so it will override that one if
        -- they both match.
        , rule "entering dark places"
            { trigger = WorldModel.MatchAny [ WorldModel.HasTag "location", WorldModel.HasTag "dark" ]
            , conditions = []
            , changes = []
            , narrative = "You can't see anything at all in there.  Better find some kind of light before going in."
            }

        -- Notice how this rule is just like the one above, but has an extra
        -- condition, so this is the one that would apply if it matches.
        -- Also note how this rule will apply if the player has the torch, but not
        -- the lighter.
        , rule "entering dark places with a light source"
            { trigger = WorldModel.MatchAny [ WorldModel.HasTag "location", WorldModel.HasTag "dark" ]
            , conditions =
                [ WorldModel.MatchAny
                    [ WorldModel.HasStat "illumination" GT 5
                    , WorldModel.HasLink "location" (WorldModel.Match "PLAYER" [])
                    ]
                ]
            , changes = [ WorldModel.Update "PLAYER" [ WorldModel.SetLink "location" "$" ] ]
            , narrative = "You can see well enough to enter this dark space."
            }
        ]


{-| Our model holds our world model, as well as any other state needed for the game.
In this simple example, the only other state we have is our story text.
-}
type alias Model =
    { worldModel : MyWorldModel
    , story : String
    }


initialModel : Model
initialModel =
    { worldModel = initialWorldModel
    , story = "You are a (mostly) brave adventurer, searching the lands for bags of gold.  You have climbed this mountain and arrived at the mouth of a cave."
    }


{-| We only track a single message - interacting with an entity. But you could have
other messages as well if desired.
-}
type Msg
    = InteractWith WorldModel.ID



-- a couple of helpers to lookup name/description info from an entity ID


getDescription : WorldModel.ID -> MyWorldModel -> String
getDescription entityID worldModel_ =
    Dict.get entityID worldModel_
        |> Maybe.map .description
        |> Maybe.withDefault ("ERROR can't find description for " ++ entityID)


getName : WorldModel.ID -> MyWorldModel -> String
getName entityID worldModel_ =
    Dict.get entityID worldModel_
        |> Maybe.map .name
        |> Maybe.withDefault ("ERROR can't find name for " ++ entityID)


{-| We update our game whenever the player clicks on an entity. We need to check if
any of our rules triggered, and if so, we need to apply the changes, and set the new
story text.
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        InteractWith entityID ->
            case Rules.findMatchingRule entityID rules model.worldModel of
                Just ( ruleID, { changes, narrative } ) ->
                    -- we could change something else in the model based on the
                    -- matched RuleID, but in this case we just log it to help with
                    -- debuggin
                    let
                        debug =
                            Debug.log "Matched rule: " ruleID
                    in
                    { model
                        | worldModel = WorldModel.applyChanges changes entityID model.worldModel
                        , story = narrative
                    }

                Nothing ->
                    -- no rule matched, so lets just show the description of the
                    -- entity that the player interacted with
                    { model | story = getDescription entityID model.worldModel }


{-| You can build your view how ever you want, querying the world model as needed.
Here we just build a very simple example.
-}
view : Model -> Html Msg
view model =
    let
        -- we can get links and stats directly
        currentLocation =
            WorldModel.getLink "PLAYER" "location" model.worldModel
                |> Maybe.withDefault "ERROR"

        fearLevel =
            WorldModel.getStat "PLAYER" "fear" model.worldModel
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "ERROR can't find PLAYER's fear stat"

        -- we can query the world model as needed
        inventory =
            WorldModel.query [ WorldModel.HasTag "item", WorldModel.HasLink "location" (WorldModel.Match "PLAYER" []) ] model.worldModel

        locations =
            WorldModel.query [ WorldModel.HasTag "location" ] model.worldModel
                |> List.filter (\( locationID, _ ) -> locationID /= currentLocation)

        items =
            WorldModel.query [ WorldModel.HasTag "item", WorldModel.HasLink "location" (WorldModel.Match currentLocation []) ] model.worldModel

        characters =
            WorldModel.query [ WorldModel.HasTag "character", WorldModel.HasLink "location" (WorldModel.Match currentLocation []) ] model.worldModel

        -- we can also assert against the world model
        isQuestComplete =
            WorldModel.assert "PLAYER" [ WorldModel.HasStat "bagsOfGoldCollected" GT 0 ] model.worldModel
    in
    div [ style "width" "70%", style "margin" "auto" ]
        [ h1 [] [ text <| "You are currently located in the " ++ getName currentLocation model.worldModel ]
        , h2 [] [ text <| getDescription currentLocation model.worldModel ]
        , h3 [] [ text <| "Fear level: " ++ fearLevel ]
        , div [ style "display" "flex" ]
            [ div [ style "flex" "0 0 auto" ]
                [ h3 [] [ text "You have:" ]
                , div [] <| List.map entityView inventory
                , h3 [] [ text "You see the following items:" ]
                , div [] <| List.map entityView items
                , h3 [] [ text "You see the following characters:" ]
                , div [] <| List.map entityView characters
                , h3 [] [ text "Places near by:" ]
                , div [] <| List.map entityView locations
                ]
            , div [ style "flex" "1 1 auto", style "font-size" "2em", style "padding" "0 2em" ]
                [ em [] [ text model.story ]
                , if isQuestComplete then
                    p [] [ strong [] [ text "Congratulations, you win!" ] ]

                  else
                    -- render nothing
                    text ""
                ]
            ]
        ]


{-| This simply uses our name/descriptions component to show the name of the entity.
-}
entityView : ( WorldModel.ID, MyEntity ) -> Html Msg
entityView ( id, { name, description } ) =
    li [ onClick <| InteractWith id, style "cursor" "pointer" ] [ text name ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
