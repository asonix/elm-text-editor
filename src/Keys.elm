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


module Keys
    exposing
        ( KeyCmd(..)
        , ToggleCmd(..)
        , ArrowCmd(..)
        , ModifierCmd(..)
        , Modifiers
        , handleCode
        , defaultModifiers
        , viewDebug
        )

{-|
@docs KeyCmd, ToggleCmd, ArrowCmd, ModifierCmd

@docs handleCode, Modifiers, defaultModifiers
@docs viewDebug
-}

-- Imports

import Html exposing (..)


-- Types


{-| KeyCmd is a command resulting from a keypress
-}
type KeyCmd
    = Delete
    | NewLine
    | NewParagraph
    | ModifierType ModifierCmd
    | ArrowType ArrowCmd
    | ToggleType ToggleCmd


{-| Modifier subcommands
-}
type ModifierCmd
    = Ctrl
    | Alt
    | Shift


{-| Arrow subcommands
-}
type ArrowCmd
    = Left
    | Up
    | Down
    | Right


{-| Toggle subcommands
-}
type ToggleCmd
    = ToggleCode
    | ToggleImage
    | ToggleLink
    | ToggleHeading
    | ToggleBold
    | ToggleItalic
    | ToggleUnderline
    | ToggleStrike


{-| Modifiers are keys that, when held, change the command caused by other keys
-}
type alias Modifiers =
    { ctrl : Bool
    , alt : Bool
    , shift : Bool
    }


{-| Initialize modifiers with sane defaults
-}
defaultModifiers : Modifiers
defaultModifiers =
    { ctrl = False
    , alt = False
    , shift = False
    }



-- Functions


{-| handleCode returns a KeyCmd representing the previous keyboard event
-}
handleCode : Modifiers -> Int -> Maybe KeyCmd
handleCode modifiers key_code =
    case key_code of
        8 ->
            Just Delete

        16 ->
            Just (ModifierType Shift)

        17 ->
            Just (ModifierType Ctrl)

        18 ->
            Just (ModifierType Alt)

        _ ->
            handleToggles modifiers key_code


handleToggles : Modifiers -> Int -> Maybe KeyCmd
handleToggles modifiers key_code =
    if modifiers.alt then
        case key_code of
            66 ->
                -- B
                Just (ToggleType ToggleBold)

            67 ->
                -- C
                Just (ToggleType ToggleCode)

            72 ->
                -- H
                Just (ToggleType ToggleHeading)

            73 ->
                -- I
                Just (ToggleType ToggleItalic)

            76 ->
                -- L
                Just (ToggleType ToggleLink)

            80 ->
                -- I
                Just (ToggleType ToggleImage)

            83 ->
                -- S
                Just (ToggleType ToggleStrike)

            85 ->
                -- U
                Just (ToggleType ToggleUnderline)

            _ ->
                Nothing
    else
        case key_code of
            13 ->
                -- Enter
                Just
                    (if modifiers.shift then
                        NewLine
                     else
                        NewParagraph
                    )

            37 ->
                Just (ArrowType Left)

            38 ->
                Just (ArrowType Up)

            39 ->
                Just (ArrowType Right)

            40 ->
                Just (ArrowType Down)

            _ ->
                Nothing


{-| render debug information
-}
viewDebug : Modifiers -> Html msg
viewDebug mods =
    Html.blockquote []
        [ h3 [] [ text "Modifiers" ]
        , p []
            [ Html.text "ctrl: "
            , Html.text
                (if mods.ctrl then
                    "True"
                 else
                    "False"
                )
            ]
        , p []
            [ Html.text "alt: "
            , Html.text
                (if mods.alt then
                    "True"
                 else
                    "False"
                )
            ]
        , p []
            [ Html.text "shift: "
            , Html.text
                (if mods.shift then
                    "True"
                 else
                    "False"
                )
            ]
        ]
