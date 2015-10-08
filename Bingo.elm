module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import List exposing (map)
import StartApp
import Signal exposing (Address)
import BingoUtils as Utils


-- MODEL

type alias Entry = {
  phrase: String,
  points: Int ,
  wasSpoken: Bool,
  id: Int
}


type alias Model =
  {
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextID: Int
  }


newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  { phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }


initialModel : Model
initialModel =
  { entries =
    [ newEntry "Doing Agile" 200 2,
      newEntry "In The Cloud" 300 3,
      newEntry "Future-Proof" 1000 1,
      newEntry "Rock-star" 400 4
    ],
    phraseInput = "",
    pointsInput = "",
    nextID = 5
  }


-- UPDATE


type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | Add

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Sort ->
      { model | entries <- List.sortBy .points model.entries }

    Delete id ->
      let
        remainingEntries =
          List.filter (\entry -> entry.id /= id) model.entries
      in
        { model | entries <- remainingEntries }

    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | wasSpoken <- (not e.wasSpoken)} else e
      in
        { model | entries <- List.map updateEntry model.entries }

    UpdatePhraseInput val ->
      { model | phraseInput <- val }

    UpdatePointsInput val ->
      { model | pointsInput <- val }

    Add ->
      let
        generateEntry =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextID
        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model
        then model
        else
          {
            model |
              phraseInput <- "",
              pointsInput <- "",
              nextID <- model.nextID + 1,
              entries <- model.entries ++ [generateEntry]
          }


--VIEW


title : String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader : Html
pageHeader =
  h1 [ id "logo", class "title" ] [ title "Yo!" 3 ]


pageFooter : Html
pageFooter =
  footer [ ]
    [ a [ href "http://willmcneilly.com" ]
        [ text "willmcneilly.com" ]
    ]


entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li
    [ classList [ ("highlight", entry.wasSpoken) ],
      onClick address (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text entry.phrase ],
      span [ class "points" ] [ text (toString entry.points) ],
      button
        [ class "delete", onClick address (Delete entry.id) ]
        [  ]
    ]


totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEntries = List.filter (\e -> e.wasSpoken == True) entries
  in
    List.sum (List.map .points spokenEntries)


totalItem : Int -> Html
totalItem total =
  li [ class "total" ]
    [ span [ class "label" ] [ text "Total" ],
      span [ class "points" ] [ text (toString total) ]
    ]


entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = map (entryItem address) entries
    allItems = entryItems ++ [ totalItem ( totalPoints entries ) ]
  in
    ul [ ] allItems


entryForm : Address Action -> Model -> Html
entryForm address model =
  div []
    [ input
        [ type' "text",
          placeholder "Input Phrase",
          value model.phraseInput,
          name "phrase",
          autofocus True,
          Utils.onInput address UpdatePhraseInput
        ]
        [],
      input
        [ type' "number",
          placeholder "Points",
          value model.pointsInput,
          name "points",
          Utils.onInput address UpdatePointsInput
        ]
        [],
      button
        [ class "add",
          onClick address Add
        ]
        [ text "Add"]
    ]


view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
    [ pageHeader,
      entryForm address model,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "SORT" ],
      pageFooter
    ]


-- WIRING


main : Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }
