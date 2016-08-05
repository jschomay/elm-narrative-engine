module Scenes exposing (..)


type alias Scenes a =
    List (Scene a)


type alias Scene a =
    List (StoryRule a)


type StoryRule a
    = StoryRule (Given a) (Then a)


type Given a
    = Given (Trigger a) (Condition a)


type Then a
    = Then (List (ChangeWorldCommand a)) NarrateCommand


type Trigger a
    = EveryTurn
    | AfterTurn Int
    | UntilTurn Int
    | OnTurn Int
    | InteractionWith a
    | FirstInteractionWith a


type Condition a
    = Always
    | With (List a)
    | WithOut (List a)


type ChangeWorldCommand a
    = LoadScene a
    | MoveTo a
    | AddLocation a
    | RemoveLocation a
    | AddInventory a
    | RemoveInventory a
    | AddCharacter a
    | RemoveCharacter a
    | AddProp a
    | RemoveProp a
    | EndStory


type NarrateCommand
    = Narrate DisplayText


type DisplayText
    = Just String
    | InOrder (List String)
