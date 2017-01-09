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
  { content: Content
  , cursor_position: CursorPosition
  , mods: Modifiers
  , input: String
  }


type alias CursorPosition =
  { paragraph: Int
  , style: Int
  , character: Int
  }


type alias Paragraph = Array Style
type alias Content = Array Paragraph


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


delFromArray : Int -> Array a -> Array a
delFromArray index array =
  Array.append
    (Array.slice 0 index array)
    (Array.slice (index+1) (Array.length array) array)


deleteStyleFromContent : Int -> Int -> Content -> Content
deleteStyleFromContent paragraph_index style_index content =
  case Array.get paragraph_index content of
    Just paragraph ->
      paragraph
        |> delFromArray style_index
        |> (flip (Array.set paragraph_index) content)

    Nothing ->
      content


serializeContent : (List a -> b) -> (Style -> a) -> Content -> List b
serializeContent transform inner_map content_structure =
  let
      scl x =
        Array.map inner_map x
          |> Array.toList
  in
      Array.map (transform << scl) content_structure
        |> Array.toList


-- INIT

init : (Model, Cmd Msg)
init = ({ content = Array.fromList
                      [ Array.fromList
                          [ Text ""
                          ]
                      ]
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
      (inputToContent text model, Cmd.none)


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


inputToContent : String -> Model -> Model
inputToContent input model =
    let
        paragraph_index = model.cursor_position.paragraph

        style_index = model.cursor_position.style

        maybe_updated_content =
          modifyStyleInContent model.content paragraph_index style_index (appendText input)

        cursor = model.cursor_position
    in
        case maybe_updated_content of
          Just updated_content ->
            { model
                | content = updated_content
                , input = ""
                , cursor_position =
                    { cursor |
                        character = cursor.character + String.length input
                    }
            }

          Nothing ->
            model


fromMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
fromMaybe fn = fromMaybeWithDefault fn Nothing


fromMaybeWithDefault : (a -> b) -> b -> Maybe a -> b
fromMaybeWithDefault fn default maybe_item =
  case maybe_item of
    Just item ->
      fn item

    Nothing ->
      default

fromTwoMaybes : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
fromTwoMaybes fn maybe_one maybe_two =
  fromTwoMaybesWithDefault fn Nothing maybe_one maybe_two

fromTwoMaybesWithDefault : (a -> b -> c) -> c -> Maybe a -> Maybe b -> c
fromTwoMaybesWithDefault fn default maybe_one maybe_two =
  fromMaybeWithDefault
    (\x -> fromMaybeWithDefault (fn x) default maybe_two)
    default
    maybe_one


delete : Model -> Model
delete model =
  let
      cursor =
        ( model.cursor_position.paragraph
        , model.cursor_position.style
        , model.cursor_position.character
        )
  in
      case cursor of
        (0, 0, 0) -> -- paragraph, style, and character are 0
          model

        (_, 0, 0) -> -- style and character are 0, paragraph is not 0
          mergeParagraphs model

        (_, _, 0) -> -- character is 0, style is not 0, paragraph is anything
          deleteFromPreviousStyle model

        _ -> -- character is not 0, style and paragraph are anything
          deleteFromStyle model


-- Should only be called when model.cursor_position.style is 0
-- AND model.cursor_position.character is 0
mergeParagraphs : Model -> Model
mergeParagraphs model =
  let
      paragraph_index =
        model.cursor_position.paragraph

      maybe_current_paragraph =
        Array.get paragraph_index model.content

      maybe_previous_paragraph =
        Array.get (paragraph_index - 1) model.content

      maybe_merged_paragraphs =
        fromTwoMaybes (curry (Just << uncurry Array.append)) maybe_previous_paragraph maybe_current_paragraph

      maybe_updated_content =
        case maybe_merged_paragraphs of
          Just merged_paragraphs ->
            model.content
              |> Array.set (paragraph_index - 1) merged_paragraphs
              |> delFromArray paragraph_index
              |> Just

          Nothing ->
            Nothing

      style_index =
        maybe_previous_paragraph
          |> fromMaybeWithDefault (flip (-) 1 << Array.length) 0

      character_index =
        maybe_previous_paragraph
          |> fromMaybe (Array.get style_index)
          |> fromMaybe (Just << String.length << getText)
          |> fromMaybeWithDefault identity 0

      cursor =
        model.cursor_position

      updated_cursor =
        { cursor
            | paragraph = cursor.paragraph - 1
            , style = style_index
            , character = character_index
        }

      updateModel updated_content =
        { model
            | content = updated_content
            , cursor_position = updated_cursor
        }
  in
      if model.cursor_position.character == 0 && model.cursor_position.style == 0 then
        maybe_updated_content
          |> fromMaybeWithDefault (updateModel) model

      else
        model


getStyleFromContent : Content -> Int -> Int -> Maybe (Paragraph, Style)
getStyleFromContent content paragraph_index style_index =
  let
      maybe_paragraph =
        Array.get paragraph_index content

      maybe_style =
        maybe_paragraph
          |> fromMaybe (Array.get style_index)
  in
      fromTwoMaybes (curry Just) maybe_paragraph maybe_style


setStyleInContent : Content -> Int -> Int -> Style -> Content
setStyleInContent content paragraph_index style_index updated_style =
  let
      maybe_paragraph =
        Array.get paragraph_index content

      maybe_updated_paragraph =
        maybe_paragraph
          |> fromMaybe (Just << Array.set style_index updated_style)
  in
      maybe_updated_paragraph
        |> fromMaybeWithDefault (flip (Array.set paragraph_index) content) content


modifyStyleInContent : Content -> Int -> Int -> (Style -> Style) -> Maybe (Content)
modifyStyleInContent content paragraph_index style_index fn =
  getStyleFromContent content paragraph_index style_index
    |> fromMaybe (Just << Tuple.second)
    |> fromMaybe (Just << fn)
    |> fromMaybe (Just << setStyleInContent content paragraph_index style_index)


deleteCharFromString : Int -> String -> String
deleteCharFromString index str =
  String.left (index - 1) str ++ (String.dropLeft index str)


deleteCharFromStyle : Int -> Style -> Style
deleteCharFromStyle index style =
  style
    |> getText
    |> deleteCharFromString index
    |> (flip (updateText) style)


getCurrentStyle : Model -> Style
getCurrentStyle model =
  let
      maybe_current_style =
        getStyleFromContent
          model.content
          model.cursor_position.paragraph
          model.cursor_position.style
  in
      maybe_current_style
        |> fromMaybeWithDefault (Tuple.second) (Text "")


-- Should only be called when model.cursor_position.character is 0
-- AND when model.cursor_position.style is not 0
deleteFromPreviousStyle : Model -> Model
deleteFromPreviousStyle model =
  let
      previous_style_index = model.cursor_position.style - 1

      getStyleLength =
        fromMaybeWithDefault (String.length << getText << Tuple.second) 0
          << getStyleFromContent model.content model.cursor_position.paragraph

      previous_style_length =
        getStyleLength previous_style_index

      current_style_length =
        getStyleLength model.cursor_position.style

      updated_content =
        deleteStyleFromContent
          model.cursor_position.paragraph
          model.cursor_position.style
          model.content

      cursor = model.cursor_position

      updated_cursor =
        { cursor
            | style = previous_style_index
            , character = previous_style_length
        }

      updated_model =
        case current_style_length of
          0 ->
            { model
                | content = updated_content
                , cursor_position = updated_cursor
            }

          _ ->
            { model
                | cursor_position = updated_cursor
            }
  in
      if cursor.character == 0 && cursor.style /= 0 then
        deleteFromStyle updated_model

      else
        model


-- Should only be called when model.cursor_position.character is not 0
deleteFromStyle : Model -> Model
deleteFromStyle model =
  let
      paragraph_index =
        model.cursor_position.paragraph

      style_index =
        model.cursor_position.style

      character_index =
        model.cursor_position.character

      maybe_updated_content =
        modifyStyleInContent
          model.content
          paragraph_index
          style_index
          (deleteCharFromStyle character_index)

      cursor =
        model.cursor_position

      updated_cursor =
        { cursor | character = character_index - 1 }

      updateModel updated_content =
        { model
            | content = updated_content
            , cursor_position = updated_cursor
        }
  in
      if model.cursor_position.character == 0 then
        model

      else
        maybe_updated_content
          |> fromMaybeWithDefault (updateModel) model



newParagraph : Model -> Model
newParagraph model =
  let
      maybe_paragraph =
        Array.get model.cursor_position.paragraph model.content

      maybe_style =
        maybe_paragraph
          |> fromMaybe (Array.get model.cursor_position.style)

      maybe_text =
        maybe_style
          |> fromMaybe (Just << getText)

      maybe_text_after =
        maybe_text
          |> fromMaybe (Just << String.dropLeft model.cursor_position.character)

      maybe_text_before =
        case maybe_text of
          Just text ->
            case String.left model.cursor_position.character text of
              "" ->
                Nothing

              text ->
                Just text

          Nothing ->
            Nothing

      style_text text =
        fromTwoMaybesWithDefault
          (curry (Array.repeat 1 << uncurry updateText))
          Array.empty
          text
          maybe_style

      more_styles =
        case maybe_paragraph of
          Just paragraph ->
            Array.slice (model.cursor_position.style + 1) (Array.length paragraph) paragraph

          Nothing ->
            Array.empty

      first_styles =
        maybe_paragraph
          |> fromMaybeWithDefault (Array.slice 0 model.cursor_position.style) Array.empty

      defaultIfEmpty array =
        if Array.isEmpty array then
          Array.fromList [updateText "" (getCurrentStyle model)]

        else
          array

      new_paragraph =
        Array.empty
          |> Array.append more_styles
          |> Array.append (style_text maybe_text_after)
          |> defaultIfEmpty

      previous_paragraph =
        Array.empty
          |> Array.append (style_text maybe_text_before)
          |> Array.append first_styles
          |> defaultIfEmpty

      updated_content =
        model.content
          |> Array.push new_paragraph
          |> Array.set model.cursor_position.paragraph previous_paragraph

      cursor = model.cursor_position
  in
      { model
          | content = updated_content
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


toggleModelStyle : Model -> (Style -> Style) -> Model
toggleModelStyle model toggleStyle =
  let
      cursor = model.cursor_position

      current_style =
        model
          |> getCurrentStyle

      new_styles =
        current_style
          |> toggleStyle
          |> (updateText "")

      maybe_paragraph =
        model.content
          |> Array.get cursor.paragraph

      maybe_style =
        fromMaybe (Array.get cursor.style) maybe_paragraph

      text =
        fromMaybeWithDefault (getText) "" maybe_style

      left_text =
        String.left cursor.character text

      right_text =
        String.dropLeft cursor.character text

      push_if_not_empty string array =
        if String.isEmpty string then
          array

        else
          Array.push (updateText string current_style) array

      new_array =
        Array.empty
          |> push_if_not_empty left_text
          |> Array.push new_styles
          |> push_if_not_empty right_text

      maybe_updated_paragraph =
        case maybe_paragraph of
          Just paragraph ->
            Array.empty
              |> Array.append (Array.slice (cursor.style+1) (Array.length paragraph) paragraph)
              |> Array.append new_array
              |> Array.append (Array.slice 0 (cursor.style) paragraph)
              |> Just

          Nothing ->
            Nothing

      updated_content =
        maybe_updated_paragraph
          |> fromMaybeWithDefault (flip (Array.set cursor.paragraph) model.content) model.content
  in
      if String.isEmpty left_text then
        { model | content = updated_content }
      else
        { model
            | content = updated_content
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
        serializeContent
          (p [])
          (renderStyle)
          (model.content)
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
        serializeContent
          (p [])
          (text << serializeToString)
          model.content
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
