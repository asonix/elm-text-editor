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
import Array exposing (..)
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
  { current_content: Array (Array Content)
  , current_styles: Content
  , mods: Modifiers
  }


getArrayLast : Array a -> Maybe a
getArrayLast array =
  case Array.length array of
    0 ->
      Nothing

    length ->
      Array.get (length - 1) array


setArrayLast : Array a -> a -> Array a
setArrayLast array item =
  case Array.length array of
    0 ->
      Array.empty |> Array.push item

    length ->
      Array.set (length-1) item array


delArrayLast : Array a -> Array a
delArrayLast array =
  Array.slice 0 (Array.length array - 1) array


serializeContentArrays : (List a -> b) -> (Content -> a) -> Array (Array Content) -> List b
serializeContentArrays transform inner_map content_structure =
  let
      scl x =
        Array.map inner_map x
          |> Array.toList
  in
      Array.map (\contents -> transform (scl contents)) content_structure
        |> Array.toList


addCurrentStylesToContent : Model -> Array (Array Content)
addCurrentStylesToContent model =
  let
      last_content = model.current_content |> getArrayLast

      new_content =
        case last_content of
          Just content ->
            Array.push model.current_styles content 

          Nothing ->
            Array.fromList [model.current_styles]

      updated_content =
        setArrayLast model.current_content new_content
  in
        updated_content


-- INIT

init : (Model, Cmd Msg)
init = ({ current_content = Array.empty
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
      case handleCode model.mods key_code of
        Just Delete ->
          (delete model, Cmd.none)

        Just Ctrl ->
          ({model | mods = {mods | ctrl = True}}, Cmd.none)

        Just Alt ->
          ({model | mods = {mods | alt = True}}, Cmd.none)

        Just Shift ->
          ({model | mods = {mods | shift = True}}, Cmd.none)

        Just NewLine ->
          (model, Cmd.none)

        Just NewParagraph ->
          (newParagraph model, Cmd.none)

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


fromMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
fromMaybe fn item =
  case item of
    Just thing ->
      fn thing

    Nothing ->
      Nothing


delete : Model -> Model
delete model =
  let
      setContentLast =
        setArrayLast model.current_content

      justRemoveNestedLast x =
        Just (x |> delArrayLast |> setContentLast)

      last_style =
        model.current_content
          |> getArrayLast
          |> fromMaybe (getArrayLast)

      content_without_last_style =
        model.current_content
          |> getArrayLast
          |> fromMaybe (justRemoveNestedLast)
  in
      if Styles.isEmpty model.current_styles then
        case content_without_last_style of
          Just content ->
            case last_style of
              Just style ->
                { model
                    | current_styles = style
                    , current_content = content
                }

              Nothing ->
                { model
                    | current_content =
                        model.current_content
                        |> delArrayLast
                }
                |> delete

          Nothing ->
            model

      else
        model


newParagraph : Model -> Model
newParagraph model =
  let
      updated_content = addCurrentStylesToContent model

      updated_styles = updateText "" model.current_styles
  in
      { model
          | current_styles = updated_styles
          , current_content = Array.push Array.empty updated_content
      }


keyup : Model -> Int -> (Model, Cmd Msg)
keyup model key_code =
  let
      mods = model.mods
  in
      case handleCode model.mods key_code of
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
        serializeContentArrays
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
        serializeContentArrays
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
