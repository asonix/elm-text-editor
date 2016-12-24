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
import Keyboard

-- MAIN

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

type alias Model
  = { current_content: List (List Content)
    , current_styles: List String
    , ctrl_pressed: Bool
    }


init : (Model, Cmd Msg)
init = ({ current_content = []
        , current_styles = []
        , ctrl_pressed = False
        }, Cmd.none)


-- UPDATE

type Msg
  = KeyDown Keyboard.KeyCode
  | KeyUp Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      case code of
        17 ->
          ({model | ctrl_pressed = True}, Cmd.none)

        _ ->
          if model.ctrl_pressed then
            (activateMode code model, Cmd.none)

          else
            (model, Cmd.none)

    KeyUp code ->
      case code of
        17 ->
          ({model | ctrl_pressed = False}, Cmd.none)

        _ ->
          (model, Cmd.none)


activateMode : Int -> Model -> Model
activateMode code model =
  case code of
    66 -> -- B
      toggleModelStyle "bold" model

    67 -> -- C
      toggleModelStyle "code" model

    72 -> -- H
      toggleModelStyle "heading" model

    73 -> -- I
      toggleModelStyle "italic" model

    76 -> -- L
      toggleModelStyle "link" model

    80 -> -- P
      toggleModelStyle "image" model

    83 -> -- S
      toggleModelStyle "strikethrough" model

    85 -> -- U
      toggleModelStyle "underline" model

    _ ->
      model


toggleModelStyle : String -> Model -> Model
toggleModelStyle style model =
  { model | current_styles = toggleStyle style model.current_styles }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ text "hello world"
        ]
    , showModel model
    ]


showModel : Model -> Html Msg
showModel model =
  div [ class "debug" ]
    [ p []
        [ text (if model.ctrl_pressed then "True" else "False")
        ]
    , p [] (List.map (\elem -> text elem) model.current_styles)
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
