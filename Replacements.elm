module Replacements (generateSubstitutions) where

import Dict exposing (Dict)
import Array exposing (Array)
import Set
import Random.PCG as Random
import Regex
import Common exposing (Action(..), Model)


{-| Run find and replace on a string
-}
replaceString : ( String, String ) -> String -> String
replaceString ( search, replacement ) string =
  Regex.replace Regex.All (Regex.regex search) (\_ -> replacement) string


{-| Randomly select one string from list
-}
sample : List String -> Random.Generator String
sample strings =
  Random.int 0 (List.length strings - 1)
    |> Random.map
        (\i ->
          Array.get i (Array.fromList strings) |> Maybe.withDefault ""
        )


{-| Run find and replace, selecting a replacement randomly from list of strings
-}
replaceStringRandomly : ( String, List String ) -> String -> Random.Generator String
replaceStringRandomly ( search, replacements ) string =
  sample replacements
    |> Random.map (\replacement -> replaceString ( search, replacement ) string)


{-| Run find and replace on multiple search strings, and randomly select replacements from list
-}
replaceAllRandomly : String -> List ( String, List String ) -> Random.Generator String
replaceAllRandomly string replacements =
  case replacements of
    [] ->
      Random.constant string

    x :: xs ->
      replaceStringRandomly x string `Random.andThen` \newString -> replaceAllRandomly newString xs


{-| Run find and replace on multiple search screens multiple times for a given string, until the string no longer
changes.
-}
replaceAllRandomlyRepeat : String -> List ( String, List String ) -> Random.Generator String
replaceAllRandomlyRepeat string replacements =
  replaceAllRandomly string replacements
    `Random.andThen` (\replacedString ->
                        if string == replacedString then
                          Random.constant string
                        else
                          replaceAllRandomlyRepeat replacedString replacements
                     )


{-| Get a liststring substitutions.
-}
generateSubstitutions : Dict String (List String) -> String -> Random.Generator (List String)
generateSubstitutions substitutions string =
  replaceAllRandomlyRepeat string (Dict.toList substitutions)
    |> Random.list 100
    |> Random.map unique


{-| Remove duplicates from a list without sorting it.
-}
unique : List comparable -> List comparable
unique =
  let
    helper set things =
      case things of
        [] ->
          []

        y :: ys ->
          if Set.member y set then
            helper set ys
          else
            y :: helper (Set.insert y set) ys
  in
    helper Set.empty
