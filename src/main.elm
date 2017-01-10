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
  , cursor: CursorPosition
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
serializeContent transform inner_map =
  Array.toList << Array.map (transform << Array.toList << Array.map inner_map)


-- INIT

init : (Model, Cmd Msg)
init = ({ content =
          Array.fromList
            [ Array.fromList
                [ Text ""
                ]
            ]
        , mods =
          { ctrl = False
          , alt = False
          , shift = False
          }
        , cursor =
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
      mods : Modifiers
      mods = model.mods

      toggle : (Style -> Style) -> Model
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
        cursor : CursorPosition
        cursor = model.cursor

        maybe_updated_content : Maybe Content
        maybe_updated_content =
          modifyStyleInContent model.content cursor.paragraph cursor.style (appendText input)
    in
        case maybe_updated_content of
          Just updated_content ->
            { model
                | content = updated_content
                , input = ""
                , cursor =
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
      cursor : (Int, Int, Int)
      cursor =
        ( model.cursor.paragraph
        , model.cursor.style
        , model.cursor.character
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


-- Should only be called when model.cursor.style is 0
-- AND model.cursor.character is 0
mergeParagraphs : Model -> Model
mergeParagraphs model =
  let
      cursor : CursorPosition
      cursor = model.cursor

      maybe_previous_paragraph : Maybe Paragraph
      maybe_previous_paragraph =
        Array.get (cursor.paragraph - 1) model.content

      maybe_merged_paragraphs : Maybe Paragraph
      maybe_merged_paragraphs =
        model.content
          |> Array.get cursor.paragraph
          |> fromTwoMaybes (curry (Just << uncurry Array.append)) maybe_previous_paragraph

      updated_style_index : Int
      updated_style_index =
        maybe_previous_paragraph
          |> fromMaybeWithDefault (flip (-) 1 << Array.length) 0
          |> (+) 1

      updateModel : Content -> Model
      updateModel updated_content =
        { model
            | content = updated_content
            , cursor =
              { cursor
                  | paragraph = cursor.paragraph - 1
                  , style = updated_style_index
                  , character = 0
              }
        }
  in
      if cursor.character == 0 && cursor.style == 0 then
        case maybe_merged_paragraphs of
          Just merged_paragraphs ->
            model.content
              |> Array.set (cursor.paragraph - 1) merged_paragraphs
              |> delFromArray cursor.paragraph
              |> updateModel

          Nothing ->
            model

      else
        model


getStyleFromContent : Content -> Int -> Int -> Maybe (Paragraph, Style)
getStyleFromContent content paragraph_index style_index =
  let
      maybe_paragraph : Maybe Paragraph
      maybe_paragraph =
        Array.get paragraph_index content

      maybe_style : Maybe Style
      maybe_style =
        maybe_paragraph
          |> fromMaybe (Array.get style_index)
  in
      fromTwoMaybes (curry Just) maybe_paragraph maybe_style


setStyleInContent : Content -> Int -> Int -> Style -> Content
setStyleInContent content paragraph_index style_index updated_style =
  Array.get paragraph_index content
    |> fromMaybe (Just << Array.set style_index updated_style)
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
  (getStyleFromContent
    model.content
    model.cursor.paragraph
    model.cursor.style)
    |> fromMaybeWithDefault (Tuple.second) (Text "")


-- Should only be called when model.cursor.character is 0
-- AND when model.cursor.style is not 0
deleteFromPreviousStyle : Model -> Model
deleteFromPreviousStyle model =
  let
      cursor : CursorPosition
      cursor = model.cursor

      previous_style_index : Int
      previous_style_index = cursor.style - 1

      getStyleLength : Int -> Int
      getStyleLength =
        fromMaybeWithDefault (String.length << getText << Tuple.second) 0
          << getStyleFromContent model.content cursor.paragraph

      updated_cursor : CursorPosition
      updated_cursor =
        { cursor
            | style = previous_style_index
            , character = getStyleLength previous_style_index
        }

      updated_model : Model
      updated_model =
        case getStyleLength cursor.style of
          0 ->
            { model
                | content =
                  deleteStyleFromContent
                    cursor.paragraph
                    cursor.style
                    model.content
                , cursor = updated_cursor
            }

          _ ->
            { model
                | cursor = updated_cursor
            }
  in
      if cursor.character == 0 && cursor.style /= 0 then
        deleteFromStyle updated_model

      else
        model


-- Should only be called when model.cursor.character is not 0
deleteFromStyle : Model -> Model
deleteFromStyle model =
  let
      cursor : CursorPosition
      cursor = model.cursor

      updateModel : Content -> Model
      updateModel updated_content =
        { model
            | content = updated_content
            , cursor =
              { cursor
                  | character = cursor.character - 1
              }
        }
  in
      if cursor.character == 0 then
        model

      else
        (modifyStyleInContent
          model.content
          cursor.paragraph
          cursor.style
          (deleteCharFromStyle cursor.character))
          |> fromMaybeWithDefault (updateModel) model



newParagraph : Model -> Model
newParagraph model =
  let
      cursor : CursorPosition
      cursor = model.cursor

      maybe_paragraph_and_style : Maybe (Paragraph, Style)
      maybe_paragraph_and_style =
        getStyleFromContent model.content cursor.paragraph cursor.style

      maybe_text : Maybe String
      maybe_text =
        maybe_paragraph_and_style
          |> fromMaybe (Just << getText << Tuple.second)

      maybe_text_after : Maybe String
      maybe_text_after =
        maybe_text
          |> fromMaybe (Just << String.dropLeft cursor.character)

      maybe_text_before : Maybe String
      maybe_text_before =
        case maybe_text of
          Just text ->
            case String.left cursor.character text of
              "" ->
                Nothing

              text ->
                Just text

          Nothing ->
            Nothing

      styleText : Maybe String -> Paragraph
      styleText maybe_text =
        fromTwoMaybesWithDefault
          (curry (Array.repeat 1 << uncurry updateText))
          Array.empty
          maybe_text
          (maybe_paragraph_and_style |> fromMaybe (Just << Tuple.second))

      more_styles : Paragraph
      more_styles =
        case maybe_paragraph_and_style of
          Just (paragraph, _) ->
            Array.slice (cursor.style + 1) (Array.length paragraph) paragraph

          Nothing ->
            Array.empty

      first_styles : Paragraph
      first_styles =
        maybe_paragraph_and_style
          |> fromMaybeWithDefault (Array.slice 0 cursor.style << Tuple.first) Array.empty

      defaultIfEmpty : Paragraph -> Paragraph
      defaultIfEmpty array =
        if Array.isEmpty array then
          Array.fromList [updateText "" (getCurrentStyle model)]

        else
          array

      new_paragraph : Paragraph
      new_paragraph =
        Array.empty
          |> Array.append more_styles
          |> Array.append (styleText maybe_text_after)
          |> defaultIfEmpty

      previous_paragraph : Paragraph
      previous_paragraph =
        Array.empty
          |> Array.append (styleText maybe_text_before)
          |> Array.append first_styles
          |> defaultIfEmpty
  in
      { model
          | content =
            model.content
              |> Array.push new_paragraph
              |> Array.set cursor.paragraph previous_paragraph
          , cursor =
              { cursor
                  | paragraph = cursor.paragraph + 1
                  , style = 0
                  , character = 0
              }
      }


keyup : Model -> Int -> (Model, Cmd Msg)
keyup model key_code =
  let
      mods : Modifiers
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
      cursor : CursorPosition
      cursor = model.cursor

      maybe_paragraph_and_style : Maybe (Paragraph, Style)
      maybe_paragraph_and_style =
        getStyleFromContent
          model.content
          cursor.paragraph
          cursor.style

      current_style : Style
      current_style =
        maybe_paragraph_and_style
          |> fromMaybeWithDefault (Tuple.second) (Text "")

      text : String
      text = getText current_style

      new_style : Style
      new_style =
        current_style
          |> toggleStyle
          |> (updateText "")

      left_text : String
      left_text =
        String.left cursor.character text

      right_text : String
      right_text =
        String.dropLeft cursor.character text

      push_if_not_empty : String -> Paragraph -> Paragraph
      push_if_not_empty string array =
        if String.isEmpty string then
          array

        else
          Array.push (updateText string current_style) array

      new_array : Paragraph
      new_array =
        Array.empty
          |> push_if_not_empty left_text
          |> Array.push new_style
          |> push_if_not_empty right_text

      updated_content : Content
      updated_content =
        case maybe_paragraph_and_style of
          Just (paragraph, _) ->
            Array.empty
              |> Array.append (Array.slice (cursor.style+1) (Array.length paragraph) paragraph)
              |> Array.append new_array
              |> Array.append (Array.slice 0 (cursor.style) paragraph)
              |> flip (Array.set cursor.paragraph) model.content

          Nothing ->
            model.content
  in
      if String.isEmpty left_text then
        { model | content = updated_content }
      else
        { model
            | content = updated_content
            , cursor =
                { cursor
                    | style = cursor.style + 1
                    , character = 0
                }
        }


-- VIEW

view : Model -> Html Msg
view model =
  let
      serialized : List (Html Msg)
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
      cursor : CursorPosition
      cursor = model.cursor

      serialized : List (Html Msg)
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
              , text (toString cursor.paragraph)
              , text ", "
              , text (toString cursor.style)
              , text ", "
              , text (toString cursor.character)
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
