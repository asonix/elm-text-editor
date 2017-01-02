{-
Copyright (C) 2016  Riley Trautman

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main exposing
    (main)

{-|
@docs main
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import Styles exposing (..)
import Keys exposing (..)
import Keyboard

-- MAIN

debug : Bool
debug = False

{-| The main function of the application
-}
main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { current_content: List (List Content)
  , current_styles: Content
  , mods: Modifiers
  }


type alias Modifiers =
  { ctrl: Bool
  , alt: Bool
  , shift: Bool
  }


getListLast : List a -> Maybe a
getListLast list =
  case list of
    [] ->
      Nothing

    [item] ->
      Just item

    (x::xs) ->
      getListLast xs


setListLast : List a -> a -> List a
setListLast list item =
  case list of
    [] ->
      [item]

    [_] ->
      [item]

    (x::xs) ->
      x::setListLast xs item


delListLast : List a -> List a
delListLast list =
  case list of
    [] ->
      []

    [_] ->
      []

    (x::xs) ->
      x::delListLast xs


serializeContentLists : (List a -> b) -> (Content -> a) -> List (List Content) -> List b
serializeContentLists transform inner_map =
  let
      scl = List.map inner_map
  in
      List.map (\contents -> transform (scl contents))


addCurrentStylesToContent : Model -> List (List Content)
addCurrentStylesToContent model =
  let
      last_content = model.current_content |> getListLast

      new_content =
        case last_content of
          Just content ->
            List.append content [model.current_styles]

          Nothing ->
            [model.current_styles]

      updated_content =
        setListLast model.current_content new_content
  in
        updated_content


-- INIT

init : (Model, Cmd Msg)
init = ({ current_content = []
        , current_styles = Text ""
        , mods =
          { ctrl = False
          , alt = False
          , shift = False
          }
        }, Cmd.none)


-- UPDATE

type Msg
  = KeyDown Keyboard.KeyCode
  | KeyUp Keyboard.KeyCode
  | NewText String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      keydown model code

    KeyUp code ->
      keyup model code

    NewText text ->
      ({model | current_styles = updateText text model.current_styles}, Cmd.none)


keydown : Model -> Int -> (Model, Cmd Msg)
keydown model key_code =
  let
      mods = model.mods
      toggle = toggleModelStyle model
  in
      case handleCode model.mods.alt key_code of
        Just Delete ->
          (model, Cmd.none)

        Just Ctrl ->
          ({model | mods = {mods | ctrl = True}}, Cmd.none)

        Just Alt ->
          ({model | mods = {mods | alt = True}}, Cmd.none)

        Just Shift ->
          ({model | mods = {mods | shift = True}}, Cmd.none)

        Just ToggleCode ->
          (toggle toggleCode, Cmd.none)

        Just ToggleImage ->
          (toggle toggleImage, Cmd.none)

        Just ToggleLink ->
          (toggle (toggleLink ""), Cmd.none)

        Just ToggleHeading ->
          (toggle toggleHeading, Cmd.none)

        Just ToggleBold ->
          (toggle toggleBold, Cmd.none)

        Just ToggleItalic ->
          (toggle toggleItalic, Cmd.none)

        Just ToggleUnderline ->
          (toggle toggleUnderline, Cmd.none)

        Just ToggleStrike ->
          (toggle toggleStrike, Cmd.none)

        Nothing ->
          (model, Cmd.none)


keyup : Model -> Int -> (Model, Cmd Msg)
keyup model key_code =
  let
      mods = model.mods
  in
      case handleCode model.mods.alt key_code of
        Just Delete ->
          (model, Cmd.none)

        Just Ctrl ->
          ({model | mods = {mods | ctrl = False}}, Cmd.none)

        Just Alt ->
          ({model | mods = {mods | alt = False}}, Cmd.none)

        Just Shift ->
          ({model | mods = {mods | shift = False}}, Cmd.none)

        _ ->
          (model, Cmd.none)


toggleModelStyle : Model -> (Content -> Content) -> Model
toggleModelStyle model toggleStyle =
  let
      empty = Styles.isEmpty model.current_styles

      new_styles = model.current_styles |> toggleStyle |> (updateText "")

      updated_content = addCurrentStylesToContent model
  in
      if not empty then
        { model | current_content = updated_content, current_styles = new_styles }
      else
        { model | current_styles = toggleStyle model.current_styles }


-- VIEW

view : Model -> Html Msg
view model =
  let
      input_value = getString model.current_styles

      serialized =
        serializeContentLists
          (p [])
          (renderStyle)
          (addCurrentStylesToContent model)
  in
      div []
        [ div []
            [ input [ placeholder "hello world", onInput NewText, value input_value ] []
            , div [] serialized
            ]
        , showModel model
        ]


showModel : Model -> Html Msg
showModel model =
  let
      serialized =
        serializeContentLists
          (p [])
          (\c -> text (serializeToString c))
          model.current_content
  in
      if debug then
        div [ class "debug" ]
          [ p []
              [ text "ctrl: "
              , text (if model.mods.ctrl then "True" else "False")
              ]
          , p []
              [ text "alt: "
              , text (if model.mods.alt then "True" else "False")
              ]
          , p []
              [ text "shift: "
              , text (if model.mods.shift then "True" else "False")
              ]
          , p []
              [ text "styles on input: "
              , text (serializeToString model.current_styles)
              ]
          , div []
              [ text "all input data: "
              , div [] serialized
              ]
          ]
        else
          text ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
