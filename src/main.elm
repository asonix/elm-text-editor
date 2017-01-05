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
import Debug

-- MAIN

debug : Bool
debug = True

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
  , cursor_position: CursorPosition
  , mods: Modifiers
  , input: String
  }


type alias CursorPosition =
  { paragraph: Int
  , style: Int
  , character: Int
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
      used_paragraph = model.current_content |> Array.get model.cursor_position.paragraph

      used_style = used_paragraph |> fromMaybe (Array.get model.cursor_position.style)

      new_content =
        case used_paragraph of
          Just content ->
            case used_style of
              Just style ->
                Array.set model.cursor_position.style model.current_styles content

              Nothing ->
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
        , cursor_position =
          { paragraph = 0
          , style = 0
          , character = 0
          }
        , input = ""
        }, Cmd.none)


-- UPDATE

type Msg
  = KeyDown Keyboard.KeyCode
  | KeyPress Keyboard.KeyCode
  | KeyUp Keyboard.KeyCode
  | NewText String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      keydown model code

    KeyPress code ->
      (model, Cmd.none)

    KeyUp code ->
      keyup model code

    NewText text ->
      (inputToCurrentStyle {model | input = text}, Cmd.none)


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

        Just Left ->
          (model, Cmd.none)

        Just Up ->
          (model, Cmd.none)

        Just Right ->
          (model, Cmd.none)

        Just Down ->
          (model, Cmd.none)

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


inputToCurrentStyle : Model -> Model
inputToCurrentStyle model =
    let
        updated_style = appendText model.input model.current_styles

        cursor = model.cursor_position
    in
        { model
            | current_styles = updated_style
            , input = ""
            , cursor_position =
                { cursor |
                    character = cursor.character + 1
                }
        }


inputFromCurrentStyle : Model -> Model
inputFromCurrentStyle model =
    let
        text = getString model.current_styles

        split_point = String.length text - 1

        updated_style =
          text
            |> String.left split_point
            |> (\x -> updateText x model.current_styles)

        last_char = String.right split_point text

        cursor = model.cursor_position
    in
        { model
            | current_styles = updated_style
            , input = last_char
            , cursor_position =
                { cursor |
                    character = cursor.character - 1
                }
        }


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

      cursor = model.cursor_position
  in
      { model
          | current_styles = updated_styles
          , current_content = Array.push Array.empty updated_content
          , cursor_position =
              { cursor
                  | paragraph = cursor.paragraph + 1
                  , style = 0
                  , character = 0
              }
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

      cursor = model.cursor_position
  in
      if empty then
        { model | current_styles = toggleStyle model.current_styles }
      else
        { model
            | current_content = updated_content
            , current_styles = new_styles
            , cursor_position =
                { cursor
                    | style = cursor.style + 1
                    , character = 0
                }
        }


-- VIEW

view : Model -> Html Msg
view model =
  let
      serialized =
        serializeContentArrays
          (p [])
          (renderStyle)
          (addCurrentStylesToContent model)
  in
      div []
        [ div []
            [ input [ placeholder "hello world", onInput NewText, value model.input ] []
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
              [ text "cursor position: ("
              , text (toString model.cursor_position.paragraph)
              , text ", "
              , text (toString model.cursor_position.style)
              , text ", "
              , text (toString model.cursor_position.character)
              , text ")"
              ]
          , p []
              [ text "styles on input: "
              , text (serializeToString model.current_styles)
              ]
          , p []
              [ text "input text: "
              , text model.input
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
    , Keyboard.presses KeyPress
    , Keyboard.ups KeyUp
    ]
