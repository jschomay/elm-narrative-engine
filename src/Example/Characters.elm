module Characters exposing (..)

import StoryElements exposing (..)


type MyCharacter
    = Volunteer
    | Moderator
    | AnxiousMan


storyCharacters : CharactersInfo MyCharacter
storyCharacters tag =
    case tag of
        Volunteer ->
            characterInfo "volunteer" "He sure seems stressed.  You're not helping."

        Moderator ->
            characterInfo "moderator" "She seems fully confident in her capabilities of commanding the room, but the way she glares at you now suggests she is none to pleased with you.  Oh well."

        AnxiousMan ->
            characterInfo "Anxious old man" "He looks like a mad scientist, pacing in circles, muttering to himself about some \"lost notes\"."
