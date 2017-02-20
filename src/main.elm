{-
Copyright (C) 2017  Riley Trautman

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
import Paragraph exposing (..)
import Content exposing (..)
import Keys exposing (..)
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

type alias ContentNavigation =
  { content : Content
  , previous_text : String
  , next_text : String
  }


type alias Model =
  { content_navigation : ContentNavigation
  , mods : Modifiers
  , input : String
  }


defaultContentNavigation : ContentNavigation
defaultContentNavigation =
  { content = Content.init
  , previous_text = ""
  , next_text = ""
  }

-- INIT

init : (Model, Cmd Msg)
init =
    ( { content_navigation = defaultContentNavigation
      , mods = defaultModifiers
      , input = ""
      }
    , Cmd.none
    )


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
          (moveLeft model, Cmd.none)

        Just Up ->
          -- (moveUp model, Cmd.none)
          (model, Cmd.none)

        Just Right ->
          (moveRight model, Cmd.none)

        Just Down ->
          -- (moveDown model, Cmd.none)
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


shiftLeft : (String -> String -> (String, String))
shiftLeft previous next =
  (String.dropRight 1 previous, (String.right 1 previous) ++ next)


shiftRight : (String -> String -> (String, String))
shiftRight previous next =
  (previous ++ (String.left 1 next), String.dropLeft 1 next)


moveLeftThroughText : ContentNavigation -> ContentNavigation
moveLeftThroughText cn =
  let
      (new_previous, new_next) =
        shiftLeft cn.previous_text cn.next_text
  in
      { cn
          | previous_text = new_previous
          , next_text = new_next
      }


moveRightThroughText : ContentNavigation -> ContentNavigation
moveRightThroughText cn =
  let
      (new_previous, new_next) =
        shiftRight cn.previous_text cn.next_text
  in
      { cn
          | previous_text = new_previous
          , next_text = new_next
      }


moveLeftThroughParagraph : ContentNavigation -> ContentNavigation
moveLeftThroughParagraph cn =
  let
      content : Content
      content =
        Content.updateCurrentParagraph (Paragraph.selectPreviousStyle) cn.content

      (new_previous_text, new_next_text) =
        content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText
          |> flip (shiftLeft) ""
  in
      { cn
          | content = content
          , previous_text = new_previous_text
          , next_text = new_next_text
      }


moveRightThroughParagraph : ContentNavigation -> ContentNavigation
moveRightThroughParagraph cn =
  let
      content : Content
      content =
        Content.updateCurrentParagraph (Paragraph.selectNextStyle) cn.content

      (new_previous_text, new_next_text) =
        content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText
          |> shiftRight ""
  in
      { cn
          | content = content
          , previous_text = new_previous_text
          , next_text = new_next_text
      }


moveLeftThroughContent : ContentNavigation -> ContentNavigation
moveLeftThroughContent cn =
  let
      content : Content
      content =
        cn.content
          |> Content.selectPreviousParagraph
          |> Content.updateCurrentParagraph (Paragraph.toEnd)

      new_previous_text : String
      new_previous_text =
        content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText

      new_next_text : String
      new_next_text = ""
  in
      { cn
          | content = content
          , previous_text = new_previous_text
          , next_text = new_next_text
      }


moveRightThroughContent : ContentNavigation -> ContentNavigation
moveRightThroughContent cn =
  let
      content : Content
      content =
        cn.content
          |> Content.selectNextParagraph
          |> Content.updateCurrentParagraph (Paragraph.toStart)

      new_previous_text : String
      new_previous_text = ""

      new_next_text : String
      new_next_text =
        content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText
  in
      { cn
          | content = content
          , previous_text = new_previous_text
          , next_text = new_next_text
      }


moveLeft : Model -> Model
moveLeft model =
  let
      cn = model.content_navigation
  in
      if not (String.isEmpty cn.previous_text) then
        { model | content_navigation = moveLeftThroughText cn }

      else if Paragraph.hasPreviousStyle (Content.currentParagraph cn.content) then
        { model | content_navigation = moveLeftThroughParagraph cn }

      else if (Content.hasPreviousParagraph cn.content) then
        { model | content_navigation = moveLeftThroughContent cn }

      else
        model


moveRight : Model -> Model
moveRight model =
  let
      cn = model.content_navigation
  in
      if not (String.isEmpty cn.next_text) then
        { model | content_navigation = moveRightThroughText cn }

      else if Paragraph.hasNextStyle (Content.currentParagraph cn.content) then
        { model | content_navigation = moveRightThroughParagraph cn }

      else if (Content.hasNextParagraph cn.content) then
        { model | content_navigation = moveRightThroughContent cn }

      else
        model


inputToContent : String -> Model -> Model
inputToContent input model =
  let
      cn : ContentNavigation
      cn = model.content_navigation

      new_previous_text : String
      new_previous_text =
        cn.previous_text ++ input

      combined_text : String
      combined_text =
        new_previous_text ++ cn.next_text

      content : Content
      content =
        Content.updateCurrentParagraph
          (Paragraph.updateCurrentStyle
            (Styles.setText combined_text))
          cn.content

      new_cn : ContentNavigation
      new_cn =
        { cn
          | content = content
          , previous_text = new_previous_text
          , next_text = cn.next_text
        }
  in
      { model | content_navigation = new_cn }


delete : Model -> Model
delete model =
  let
      cn = model.content_navigation
  in
      if not (String.isEmpty cn.previous_text) then
        { model | content_navigation = deleteText cn }

      else if Paragraph.hasPreviousStyle (Content.currentParagraph cn.content) then
        { model | content_navigation = deleteAcrossStyles cn }

      else if (Content.hasPreviousParagraph cn.content) then
        { model | content_navigation = deleteAcrossParagraphs cn }

      else
        model


-- Should only be called when
--
-- String.isEmpty cn.previous_text == True &&
-- Paragraph.hasPreviousStyle (Content.currentParagraph cn.content) == False &&
-- Content.hasPreviousParagraph cn.content == True
deleteAcrossParagraphs : ContentNavigation -> ContentNavigation
deleteAcrossParagraphs cn =
  let
      new_content : Content
      new_content = mergeWithPreviousParagraph cn.content

      new_previous_text : String
      new_previous_text =
        new_content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText
  in
      { cn
          | content = new_content
          , previous_text = new_previous_text
          , next_text = ""
      }


-- Should only be called when
--
-- String.isEmpty cn.previous_text == True &&
-- Paragraph.hasPreviousStyle (Content.currentParagraph cn.content) == True
deleteAcrossStyles : ContentNavigation -> ContentNavigation
deleteAcrossStyles cn =
  let
      new_content : Content
      new_content =
        if String.isEmpty cn.next_text then
          cn.content
            |> Content.updateCurrentParagraph (Paragraph.removeCurrentStyle)
            |> Content.updateCurrentParagraph
                (Paragraph.updateCurrentStyle
                  (Styles.updateText
                    (String.dropRight 1)))
        else
          cn.content
            |> Content.updateCurrentParagraph (Paragraph.selectPreviousStyle)
            |> Content.updateCurrentParagraph
                (Paragraph.updateCurrentStyle
                  (Styles.updateText
                    (String.dropRight 1)))

      new_previous_text : String
      new_previous_text =
        new_content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.getText
  in
      { cn
          | content = new_content
          , previous_text = new_previous_text
          , next_text = ""
      }


-- Should only be called when
--
-- String.isEmpty cn.previous_text == False
deleteText : ContentNavigation -> ContentNavigation
deleteText cn =
  let
      new_previous_text : String
      new_previous_text =
        String.dropRight 1 cn.previous_text

      combined_text : String
      combined_text =
        new_previous_text ++ cn.next_text

      new_content : Content
      new_content =
        cn.content
          |> Content.updateCurrentParagraph
              (Paragraph.updateCurrentStyle
                (Styles.setText combined_text))
  in
      { cn
          | content = new_content
          , previous_text = new_previous_text
          , next_text = cn.next_text
      }


newParagraph : Model -> Model
newParagraph model =
  let
      cn : ContentNavigation
      cn = model.content_navigation

      last_style : Style
      last_style =
        cn.content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.setText cn.previous_text

      first_style : Style
      first_style =
        Styles.setText cn.next_text last_style

      next_styles : List Style
      next_styles =
        cn.content
          |> Content.currentParagraph
          |> Paragraph.nextStyles

      new_content : Content
      new_content =
        cn.content
          |> Content.updateCurrentParagraph (Paragraph.clearNextStyles)
          |> Content.updateCurrentParagraph (Paragraph.setCurrentStyle last_style)
          |> Content.insertParagraph (first_style::next_styles)

      new_cn : ContentNavigation
      new_cn =
        { cn
            | content = new_content
            , previous_text = ""
            , next_text = cn.next_text
        }
  in
      { model | content_navigation = new_cn }


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
      cn : ContentNavigation
      cn = model.content_navigation

      current_style : Style
      current_style =
        cn.content
          |> Content.currentParagraph
          |> Paragraph.currentStyleWithDefault Styles.empty
          |> Styles.setText ""

      left_style : Maybe Style
      left_style =
        if String.isEmpty cn.previous_text then
          Nothing
        else
          Just (Styles.setText cn.previous_text current_style)

      right_style : Maybe Style
      right_style =
        if String.isEmpty cn.next_text then
          Nothing
        else
          Just (Styles.setText cn.next_text current_style)

      new_current_style : Style
      new_current_style =
        current_style
          |> toggleStyle

      new_content : Content
      new_content =
        cn.content
          |> Content.updateCurrentParagraph (Paragraph.insertBefore left_style)
          |> Content.updateCurrentParagraph (Paragraph.selectNextStyle)
          |> Content.updateCurrentParagraph (Paragraph.insertAfter right_style)
          |> Content.updateCurrentParagraph (Paragraph.selectPreviousStyle)
          |> Content.updateCurrentParagraph (Paragraph.setCurrentStyle new_current_style)

      new_cn : ContentNavigation
      new_cn =
        { cn
            | content = new_content
            , previous_text = ""
            , next_text = ""
        }
  in
      { model | content_navigation = new_cn }


-- VIEW

view : Model -> Html Msg
view model =
  let
      cn : ContentNavigation
      cn = model.content_navigation

      serialized : List (Html Msg)
      serialized =
        cn.content
          |> Content.toList
          |> List.map Paragraph.toList
          |> List.map (\paragraph -> p [] (List.map (Styles.render) paragraph))
  in
      div []
        [ css "editor.css"
        , div []
            [ input [ placeholder "hello world", onInput NewText, value model.input ] []
            , div [] serialized
            ]
        , showModel model
        ]


css : String -> Html Msg
css path =
  node "link" [ rel "Stylesheet", href path ] []


showModel : Model -> Html Msg
showModel model =
  let
      s_lists : List (Html Msg)
      s_lists =
        model.content_navigation.content
          |> Content.toList
          |> List.map (List.map Styles.serializeToString << Paragraph.toList)
          |> List.map (List.map (Html.p [] << List.singleton << text))
          |> List.map (Html.blockquote [])
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
          , p [] s_lists
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
