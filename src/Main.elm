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


module Main exposing (main)

{-|
@docs main
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dom exposing (focus)
import Task exposing (perform)
import String exposing (..)
import Styles exposing (..)
import Paragraph exposing (..)
import Content exposing (..)
import Navigation exposing (..)
import Keys exposing (..)
import Keyboard
import Debug


-- MAIN


debug : Bool
debug =
    True


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
    { navigation : Navigation
    , mods : Modifiers
    , input : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { navigation = Navigation.init
      , mods = defaultModifiers
      , input = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EditorMsg EditorType


type EditorType
    = KeyDown Keyboard.KeyCode
    | KeyPress Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | NewText String
    | Focus
    | Nil


update : Msg -> Model -> ( Model, Cmd Msg )
update (EditorMsg msg) model =
    case msg of
        KeyDown code ->
            keydown model code

        KeyPress code ->
            ( model, Cmd.none )

        KeyUp code ->
            keyup model code

        NewText text ->
            ( { model
                | navigation = Navigation.takeInput text model.navigation
              }
            , Cmd.none
            )

        Focus ->
            ( model
            , Task.attempt (always (EditorMsg Nil)) (Dom.focus "editor-input")
            )

        Nil ->
            ( model, Debug.log "nil" Cmd.none )


keydown : Model -> Int -> ( Model, Cmd Msg )
keydown model key_code =
    case handleCode model.mods key_code of
        Just Delete ->
            ( { model | navigation = Navigation.delete model.navigation }
            , Cmd.none
            )

        Just NewLine ->
            ( model, Cmd.none )

        Just NewParagraph ->
            ( { model | navigation = Navigation.newParagraph model.navigation }
            , Cmd.none
            )

        Just (ModifierType mod_type) ->
            ( handleModifier mod_type model, Cmd.none )

        Just (ArrowType arrow_type) ->
            ( handleArrow arrow_type model, Cmd.none )

        Just (ToggleType toggle) ->
            ( handleToggle toggle model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handleModifier : ModifierCmd -> Model -> Model
handleModifier mod_type model =
    let
        mods : Modifiers
        mods =
            model.mods
    in
        case mod_type of
            Ctrl ->
                { model | mods = { mods | ctrl = True } }

            Alt ->
                { model | mods = { mods | alt = True } }

            Shift ->
                { model | mods = { mods | shift = True } }


handleArrow : ArrowCmd -> Model -> Model
handleArrow arrow_type model =
    case arrow_type of
        Left ->
            { model | navigation = Navigation.moveLeft model.navigation }

        Up ->
            model

        Right ->
            { model | navigation = Navigation.moveRight model.navigation }

        Down ->
            model


handleToggle : ToggleCmd -> Model -> Model
handleToggle toggle_type model =
    case toggle_type of
        ToggleCode ->
            toggleModelStyle toggleCode model

        ToggleImage ->
            toggleModelStyle toggleImage model

        ToggleLink ->
            toggleModelStyle (toggleLink "") model

        ToggleHeading ->
            toggleModelStyle toggleHeading model

        ToggleBold ->
            toggleModelStyle toggleBold model

        ToggleItalic ->
            toggleModelStyle toggleItalic model

        ToggleUnderline ->
            toggleModelStyle toggleUnderline model

        ToggleStrike ->
            toggleModelStyle toggleStrike model


keyup : Model -> Int -> ( Model, Cmd Msg )
keyup model key_code =
    let
        mods : Modifiers
        mods =
            model.mods
    in
        case handleCode model.mods key_code of
            Just (ModifierType Ctrl) ->
                ( { model | mods = { mods | ctrl = False } }, Cmd.none )

            Just (ModifierType Alt) ->
                ( { model | mods = { mods | alt = False } }, Cmd.none )

            Just (ModifierType Shift) ->
                ( { model | mods = { mods | shift = False } }, Cmd.none )

            _ ->
                ( model, Cmd.none )


toggleModelStyle : (Style -> Style) -> Model -> Model
toggleModelStyle toggle_style model =
    { model
        | navigation = Navigation.toggleStyle toggle_style model.navigation
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ css "editor.css"
        , div
            [ class "editor"
            , onClick (EditorMsg Focus)
            ]
            [ input
                [ placeholder "hello world"
                , onInput (EditorMsg << NewText)
                , value model.input
                , id "editor-input"
                ]
                []
            , Navigation.view model.navigation
            ]
        , showModel model
        ]


css : String -> Html Msg
css path =
    node "link" [ rel "Stylesheet", href path ] []


showModel : Model -> Html Msg
showModel model =
    if debug then
        div [ class "debug" ]
            [ Keys.viewDebug model.mods
            , Navigation.viewDebug model.navigation
            ]
    else
        text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (EditorMsg << KeyDown)
        , Keyboard.presses (EditorMsg << KeyPress)
        , Keyboard.ups (EditorMsg << KeyUp)
        ]
