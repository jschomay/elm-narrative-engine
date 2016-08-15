module Main exposing (..)

import Dict exposing (..)
import String exposing (..)
import Engine exposing (..)
import Engine exposing (..)
import StoryElements exposing (..)
import StoryRules exposing (..)
import StoryState exposing (..)


main : Program Never
main =
    loadStory "Stage Fright" storyElements storyRules initialStoryState


initialStoryState : StoryState MyStoryElement MyScene
initialStoryState =
    { currentLocation = Kitchen
    , currentScene = Intro
    , inventory = [ Watch ]
    , knownLocations = [ Kitchen, BackDoor ]
    , storyLine = "Well, here I am..." :: []
    , itemsByLocation = Dict.singleton (toString Kitchen) [ Envelope ]
    , charactersByLocation = Dict.singleton (toString Kitchen) [ Stranger ]
    }


storyElements : StoryElementsConfig MyStoryElement
storyElements element =
    case element of
        Envelope ->
            DisplayInformation "Unfamilar Envelope" "You find an unfamilar envelope in your pocket, stuffed with thickly folded papers."

        Kitchen ->
            DisplayInformation "Commercial kitchen" "Clean and steril, with rows of oversized ovens, stainless steel counters, and lots of pots and pans.  No chefs or cooks of any kind though."

        Stranger ->
            DisplayInformation "A stranger" "I've never seen him before, but he seems very adamant about getting my attention."

        Bob ->
            DisplayInformation "Bob" "Just a guy"

        Watch ->
            DisplayInformation "Wristwatch" "That's strange, it doesn't have any numbers on it..."

        BackDoor ->
            DisplayInformation "Back door" "It looks like an exit out of here."

        Auditorium ->
            DisplayInformation "Auditorium" "Oh crap this room is huge.  There must be over a hundred people in the audience... all looking at me expectedly.  Yikes."

        Podium ->
            DisplayInformation "Podium" "The scariest seat in the house.  Looks like it's reserved for me."

        NervousPresenter ->
            DisplayInformation "Nervous man" "Pacing back and forth, he looks even more nervous than me.  He keeps muttering to himself."

        missing ->
            DisplayInformation ((missing |> toString |> String.toUpper) ++ " name missing***") ((missing |> toString |> String.toUpper) ++ " description missing***")


storyRules : StoryRulesConfig MyStoryElement MyScene
storyRules scene =
    case scene of
        Intro ->
            introSceneRules

        ActionsTest ->
            actionsTestRules


actionsTestRules : Scene MyStoryElement MyScene
actionsTestRules =
    [ StoryRule (Given (InteractionWith Envelope) (WithOut [ Envelope ]))
        (Do [ AddInventory Envelope, RemoveProp Envelope Kitchen ] (Narrate (Simple "taking the envelope")))
    , StoryRule (Given (InteractionWith Watch) (Always))
        (Do [ RemoveInventory Watch, AddProp Watch Kitchen ] (Narrate (Simple "dropping watch")))
    , StoryRule (Given (InteractionWith Stranger) (Always))
        (Do [ AddLocation Auditorium, RemoveLocation BackDoor ] (Narrate (Simple "learn about auditorium, forget back door")))
    , StoryRule (Given (InteractionWith BackDoor) (Always))
        (Do [ AddCharacter Bob Auditorium ] (Narrate (Simple "bob is in the auditorium")))
    , StoryRule (Given (InteractionWith Kitchen) (Always))
        (Do [ MoveTo Kitchen ] (Narrate (Simple "back to kitchen")))
    , StoryRule (Given (InteractionWith Auditorium) (Always))
        (Do [ MoveTo Auditorium ] (Narrate (Simple "go to auditorium")))
    , StoryRule (Given (InteractionWith Bob) (Always))
        (Do [ RemoveCharacter Bob Auditorium ] (Narrate (Simple "bob leaves")))
    ]


introSceneRules : Scene MyStoryElement MyScene
introSceneRules =
    [ StoryRule (Given (InteractionWith Envelope) (WithOut [ Envelope ]))
        (Do [ AddInventory Envelope ] (Narrate (Simple "A mysterious envelope, I'll take that.")))
    , StoryRule (Given (InteractionWith Envelope) (In [ Auditorium ]))
        (Do [] (Narrate (Simple "Ladies and gentlemen.... my speech...")))
    , StoryRule (Given (InteractionWith Envelope) (Near [ NervousPresenter ]))
        (Do [] (Narrate (Simple "Is this yours?  Yes!! Thanks!!")))
    , StoryRule (Given (InteractionWith Auditorium) (Always))
        (Do [ MoveTo Auditorium, AddProp Podium Auditorium ] (Narrate (Simple "I hesitantly went into the auditorium...")))
    , StoryRule (Given (InteractionWith Watch) (Always))
        (Do [ LoadScene ActionsTest ] (Narrate (Simple "switch to testing scene")))
    ]


type MyScene
    = Intro
    | ActionsTest


type MyStoryElement
    = Envelope
    | Watch
    | Kitchen
    | Stranger
    | Bob
    | Podium
    | BackDoor
    | NervousPresenter
    | Auditorium
    | SomethingElse
