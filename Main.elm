module Main (..) where

import StartApp as StartApp
import Dict exposing (Dict)
import Array exposing (Array)
import Effects exposing (Never)
import Random.PCG as Random
import Set exposing (Set)
import Keyboard
import Task
import Time
import Common exposing (Action(..), Model, initialModel)
import View exposing (view)
import Replacements exposing (generateSubstitutions)


mailbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  Signal.mergeMany [ mailbox.signal, keys, animate ]


model : Signal Model
model =
  Signal.foldp update initialModel actions


main =
  Signal.map (view mailbox.address) model


keys =
  let
    getAction keycodes =
      if Set.member 13 keycodes && Set.member 18 keycodes then
        ComputeSubstitutions
      else if Set.member 72 keycodes && Set.member 18 keycodes then
        Help
      else
        NoOp
  in
    Signal.map getAction Keyboard.keysDown


animate =
  Signal.map Animate (Time.fps 60)


update : Action -> Model -> Model
update action model =
  case action of
    SetSeedString string ->
      { model | seedString = string }

    SetSearchString i string ->
      { model | searchStrings = Array.set i string model.searchStrings }

    SetReplaceStringList i strings ->
      { model | replaceStringLists = Array.set i strings model.replaceStringLists }

    ComputeSubstitutions ->
      let
        dict =
          Dict.fromList <| List.map2 (,) (Array.toList model.searchStrings) (Array.toList model.replaceStringLists)

        ( results, newSeed ) =
          Random.generate (generateSubstitutions dict model.seedString) model.seed
      in
        { model
          | help = False
          , results = results
          , seed = newSeed
        }

    Animate dt ->
      { model | t = model.t + dt }

    Help ->
      { model | help = not model.help }

    NoOp ->
      model
