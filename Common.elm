module Common (..) where

import Array exposing (Array)
import Random.PCG as Random


type Action
  = SetSeedString String
  | SetSearchString Int String
  | SetReplaceStringList Int (List String)
  | ComputeSubstitutions
  | Animate Float
  | Help
  | NoOp


type alias Model =
  { seedString : String
  , searchStrings : Array String
  , replaceStringLists : Array (List String)
  , results : List String
  , seed : Random.Seed
  , t : Float
  , help : Bool
  }


initialModel : Model
initialModel =
  { seedString = "A #noun should be treated with #kindness."
  , searchStrings = initialGroups |> List.map fst |> Array.fromList
  , replaceStringLists = initialGroups |> List.map snd |> Array.fromList
  , results = helpText
  , seed = Random.initialSeed 2024121836
  , t = 0
  , help = False
  }


nGroups =
  10


backfill : List a -> a -> Int -> List a
backfill list filler n =
  list ++ (List.repeat (n - (List.length list)) filler)


initialGroups =
  backfill
    [ ( "#noun", [ "Jedi", "Sith Lord", "zebra", "mongoose", "rattlesnake" ] )
    , ( "#kindness", [ "#something good", "#something bad" ] )
    , ( "#something good", [ "compassion", "love", "kindness", "care" ] )
    , ( "#something bad", [ "fear", "contempt", "caution" ] )
    ]
    ( "", [] )
    nGroups


helpText =
  [ "HELP"
  , "ALT + Enter = Run"
  , "ALT + h = Show or hide this Help"
  , "This tool replaces text, but unlike normal find-and-replace, it can find-and-replace multiple things at once, and can find-and-replace the replacements (by selecting randomly from some options)."
  , "How it works: write some text in the flashing black box. Write the words you want to replace in the grey boxes. Write the replacements in the red boxes. Then press ALT + Enter to get a list of results using different replacements."
  , "Here's a more detailed explanation."
  , "In find and replace, you can search for a pattern like 'dog' in some text, then replace it with cat."
  , "So, 'My dog is fast.' would turn into 'My cat is fast.'"
  , "You might also want to replace two things at once. For instance, you could replace 'dog' with 'cat', and 'fast' with 'stealthy'."
  , "So, 'My dog is fast.' would turn into 'My cat is stealthy.'"
  , "Perhaps you'd like to find a word and replace it with a word taken randomly from a list. You could replace 'dog' with either 'cat' or 'mouse'."
  , "So, 'My dog is fast.' could turn into 'My cat is fast.', but it could also turn into 'My mouse is fast.'"
  , "There's one more thing you could try. Start with the text 'My animal is fast.' Then we can replace it with either 'cat that purrs' or 'dog that barks.' After doing that, you could replace 'purrs' with either 'jumps' or 'scratches', and replace 'barks' with either 'chases cats' or 'does tricks.'"
  , "In then end, you might end up with a sentence like, 'My cat that jumps is fast.' or 'My dog that does tricks is fast."
  ]
