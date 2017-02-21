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
    = KeyDown Keyboard.KeyCode
    | KeyPress Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | NewText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            keydown model code

        KeyPress code ->
            ( model, Cmd.none )

        KeyUp code ->
            keyup model code

        NewText text ->
            ( inputToContent text model, Cmd.none )


keydown : Model -> Int -> ( Model, Cmd Msg )
keydown model key_code =
    let
        mods : Modifiers
        mods =
            model.mods

        toggle : (Style -> Style) -> Model
        toggle =
            flip (toggleModelStyle) model
    in
        case handleCode model.mods key_code of
            Just Delete ->
                ( delete model, Cmd.none )

            Just Ctrl ->
                ( { model | mods = { mods | ctrl = True } }, Cmd.none )

            Just Alt ->
                ( { model | mods = { mods | alt = True } }, Cmd.none )

            Just Shift ->
                ( { model | mods = { mods | shift = True } }, Cmd.none )

            Just Left ->
                ( moveLeft model, Cmd.none )

            Just Up ->
                -- (moveUp model, Cmd.none)
                ( model, Cmd.none )

            Just Right ->
                ( moveRight model, Cmd.none )

            Just Down ->
                -- (moveDown model, Cmd.none)
                ( model, Cmd.none )

            Just NewLine ->
                ( model, Cmd.none )

            Just NewParagraph ->
                ( newParagraph model, Cmd.none )

            Just ToggleCode ->
                ( toggle toggleCode, Cmd.none )

            Just ToggleImage ->
                ( toggle toggleImage, Cmd.none )

            Just ToggleLink ->
                ( toggle (toggleLink ""), Cmd.none )

            Just ToggleHeading ->
                ( toggle toggleHeading, Cmd.none )

            Just ToggleBold ->
                ( toggle toggleBold, Cmd.none )

            Just ToggleItalic ->
                ( toggle toggleItalic, Cmd.none )

            Just ToggleUnderline ->
                ( toggle toggleUnderline, Cmd.none )

            Just ToggleStrike ->
                ( toggle toggleStrike, Cmd.none )

            Nothing ->
                ( model, Cmd.none )


moveLeft : Model -> Model
moveLeft model =
    { model
        | navigation = Navigation.moveLeft model.navigation
    }


moveRight : Model -> Model
moveRight model =
    { model
        | navigation = Navigation.moveRight model.navigation
    }


inputToContent : String -> Model -> Model
inputToContent input model =
    { model
        | navigation = Navigation.takeInput input model.navigation
    }


delete : Model -> Model
delete model =
    { model
        | navigation = Navigation.delete model.navigation
    }


newParagraph : Model -> Model
newParagraph model =
    { model
        | navigation = Navigation.newParagraph model.navigation
    }


keyup : Model -> Int -> ( Model, Cmd Msg )
keyup model key_code =
    let
        mods : Modifiers
        mods =
            model.mods
    in
        case handleCode model.mods key_code of
            Just Delete ->
                ( model, Cmd.none )

            Just Ctrl ->
                ( { model | mods = { mods | ctrl = False } }, Cmd.none )

            Just Alt ->
                ( { model | mods = { mods | alt = False } }, Cmd.none )

            Just Shift ->
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
        , div []
            [ input
                [ placeholder "hello world"
                , onInput NewText
                , value model.input
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
    let
        s_lists : List (Html Msg)
        s_lists =
            model.navigation.content
                |> Content.toList
                |> List.map (List.map Styles.serializeToString << Paragraph.toList)
                |> List.map (List.map (Html.p [] << List.singleton << text))
                |> List.map (Html.blockquote [])
    in
        if debug then
            div [ class "debug" ]
                [ p []
                    [ text "ctrl: "
                    , text
                        (if model.mods.ctrl then
                            "True"
                         else
                            "False"
                        )
                    ]
                , p []
                    [ text "alt: "
                    , text
                        (if model.mods.alt then
                            "True"
                         else
                            "False"
                        )
                    ]
                , p []
                    [ text "shift: "
                    , text
                        (if model.mods.shift then
                            "True"
                         else
                            "False"
                        )
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