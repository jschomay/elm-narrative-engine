module Characters exposing (..)

import StoryElements exposing (..)

type MyCharacter
    = Volunteer
    | Moderator


storyCharacters : CharactersInfo MyCharacter
storyCharacters tag =
    case tag of
        Volunteer ->
            character "volunteer" "He sure seems stressed.  You're not helping."

        Moderator ->
            character "moderator" "She seems to command the audience and speakers with ease.  This definitely isn't her first rodeo."

