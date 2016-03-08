module View (..) where

import Html exposing (Html, Attribute, text, toElement, fromElement, div, input, textarea)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue, onClick, onDoubleClick)
import Signal exposing (Address)
import String
import Array exposing (Array)
import Common exposing (Action(..), Model, initialModel, nGroups, helpText)


halves list =
  let
    n =
      List.length list // 2
  in
    ( List.take n list, List.drop n list )


view : Address Action -> Model -> Html
view address model =
  div
    [ pageStyle ]
    [ -- infoPanel address model
      inputPanel address model
    , outputPanel address model
    ]


infoPanel : Address Action -> Model -> Html
infoPanel address model =
  div
    [ infoPanelStyle ]
    [ text "Combinator"
    ]


inputPanel : Address Action -> Model -> Html
inputPanel address model =
  let
    generateInputs range =
      List.map (\i -> groupInput address model i) range

    ( topRowIndices, bottomRowIndices ) =
      halves [0..(Array.length model.searchStrings) - 1]

    groupInputs1 =
      generateInputs topRowIndices

    groupInputs2 =
      generateInputs bottomRowIndices
  in
    div
      [ inputPanelStyle ]
      [ seedStringInput address model
      , div
          [ groupWrapperStyle ]
          [ div [ groupInputsStyle ] groupInputs1
          , div [ groupInputsStyle ] groupInputs2
          ]
      ]


seedStringInput : Address Action -> Model -> Html
seedStringInput address model =
  let
    opacity =
      if model.seedString == initialModel.seedString then
        toString ((sin (model.t / 500)) * 0.3 + 0.7)
      else
        "1"
  in
    input
      [ placeholder "Write a sentence here. I #verb this!"
      , on "input" targetValue (\x -> Signal.message address (SetSeedString x))
      , value model.seedString
      , seedStringInputStyle opacity
      ]
      []


groupInput : Address Action -> Model -> Int -> Html
groupInput address model i =
  div
    [ groupInputStyle ]
    [ div [ searchStringInputStyle' ] [ searchStringInput address model i ]
    , div [ replaceStringListInputStyle' ] [ replaceStringListInput address model i ]
    ]


searchStringInput : Address Action -> Model -> Int -> Html
searchStringInput address model i =
  let
    searchString =
      Array.get i model.searchStrings |> Maybe.withDefault ""
  in
    input
      [ placeholder ""
      , on "input" targetValue (\x -> Signal.message address (SetSearchString i x))
      , value searchString
      , searchStringInputStyle
          (if searchString == "" then
            "0.5"
           else
            "1"
          )
      ]
      []


replaceStringListInput : Address Action -> Model -> Int -> Html
replaceStringListInput address model i =
  let
    searchString =
      Array.get i model.searchStrings |> Maybe.withDefault ""

    valueString =
      String.join "\n" (Array.get i model.replaceStringLists |> Maybe.withDefault [])
  in
    textarea
      [ value valueString
      , on "input" targetValue (\x -> Signal.message address (SetReplaceStringList i (String.lines x)))
      , replaceStringListInputStyle
          (if searchString == "" then
            "0.5"
           else
            "1"
          )
      ]
      []


outputPanel : Address Action -> Model -> Html
outputPanel address model =
  let
    items =
      if model.help then
        helpText
      else
        model.results
  in
    div
      [ outputPanelStyle ]
      (List.map resultOutput items)


resultOutput : String -> Html
resultOutput string =
  let
    alignment =
      if string == "HELP" then
        "center"
      else
        "left"
  in
    div
      [ resultOutputStyle alignment ]
      [ text string
      ]



{- - Styles - -}


backgroundColor =
  "#103A45"


pageStyle =
  style
    [ ( "top", "0px" )
    , ( "bottom", "0px" )
    , ( "left", "0px" )
    , ( "right", "0px" )
    , ( "position", "absolute" )
    , ( "background", backgroundColor )
    , ( "box-sizing", "border-box" )
    , ( "padding", "20px" )
    ]


infoPanelStyle =
  style
    [ ( "height", "10%" )
    , ( "background-color", "white" )
    , ( "color", "B28E39" )
    , ( "text-align", "center" )
    , ( "font-weight", "bold" )
    , ( "font-size", "2em" )
    , ( "padding", "15px 15px" )
    , ( "box-sizing", "border-box" )
    ]


inputPanelStyle =
  style
    [ ( "height", "100%" )
    , ( "width", "70%" )
    , ( "box-sizing", "border-box" )
    , ( "float", "right" )
    ]


groupWrapperStyle =
  style
    [ ( "height", "45%" )
    , ( "width", "100%" )
    , ( "overflow", "clip" )
    , ( "box-sizing", "border-box" )
    ]


groupInputsStyle =
  style
    [ ( "height", "100%" )
    , ( "width", "100%" )
    , ( "box-sizing", "border-box" )
    ]


seedStringInputStyle opacity =
  style
    [ ( "background-color", "#332F2C" )
    , ( "font-size", "1.2em" )
    , ( "font-style", "italic" )
    , ( "color", "white" )
    , ( "width", "100%" )
    , ( "height", "10%" )
    , ( "float", "left" )
    , ( "border", "none" )
    , ( "padding", "10px 15px" )
    , ( "box-sizing", "border-box" )
    , ( "border-top", String.join " " [ "4px solid", backgroundColor ] )
    , ( "opacity", opacity )
    , ( "border", "solid 1px grey" )
    ]


groupInputStyle =
  style
    [ ( "width", 200 / (toFloat nGroups) |> toString |> \x -> (++) x "%" )
    , ( "height", "100%" )
    , ( "float", "left" )
    , ( "box-sizing", "border-box" )
    ]


searchStringInputStyle opacity =
  style
    [ ( "background-color", "grey" )
    , ( "font-weight", "bold" )
    , ( "color", "white" )
    , ( "width", "100%" )
    , ( "height", "100%" )
    , ( "border", "none" )
    , ( "padding", "5px 10px" )
    , ( "box-sizing", "border-box" )
    , ( "opacity", opacity )
    ]


searchStringInputStyle' =
  style
    [ ( "width", "100%" )
    , ( "height", "15%" )
    , ( "box-sizing", "border-box" )
    , ( "border-right", String.join " " [ "4px solid", backgroundColor ] )
    , ( "border-top", String.join " " [ "4px solid", backgroundColor ] )
    , ( "border-bottom", "none" )
    ]


replaceStringListInputStyle opacity =
  style
    [ ( "background-color", "#2588A2" )
    , ( "color", "white" )
    , ( "width", "100%" )
    , ( "height", "100%" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "10px 10px" )
    , ( "border", "none" )
    , ( "resize", "none" )
    , ( "font-family", "Arial" )
    , ( "opacity", opacity )
    ]


replaceStringListInputStyle' =
  style
    [ ( "width", "100%" )
    , ( "height", "85%" )
    , ( "box-sizing", "border-box" )
    , ( "border", String.join " " [ "4px solid", backgroundColor ] )
    , ( "border-left", "none" )
    ]


outputPanelStyle =
  style
    [ ( "background-color", "#332F2C" )
    , ( "width", "30%" )
    , ( "height", "100%" )
    , ( "box-sizing", "border-box" )
    , ( "float", "left" )
    , ( "border", String.join " " [ "5px solid", backgroundColor ] )
    , ( "border-left", "none" )
    , ( "overflow-y", "scroll" )
    , ( "padding", "10px 10px" )
    ]


resultOutputStyle alignment =
  style
    [ ( "background-color", "#332F2C" )
    , ( "color", "lightgrey" )
    , ( "width", "90%%" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid " ++ "grey" )
    , ( "text-align", alignment )
    ]


resultOutputStyle' alignment =
  style
    [ ( "background-color", "#332F2C" )
    , ( "color", "lightgrey" )
    , ( "width", "90%%" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "10px 10px" )
    , ( "border-bottom", "5px solid grey" )
    , ( "text-align", alignment )
    ]
