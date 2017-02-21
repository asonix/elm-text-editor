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


module Keys exposing (KeyCmd(..), Toggle(..), Modifiers, handleCode, defaultModifiers)

{-|
@docs KeyCmd, Toggle

@docs handleCode, Modifiers, defaultModifiers
-}

-- Imports
-- Types


{-| KeyCmd is a command resulting from a keypress
-}
type KeyCmd
    = Delete
    | Ctrl
    | Alt
    | Shift
    | Left
    | Up
    | Right
    | Down
    | NewLine
    | NewParagraph
    | ToggleType Toggle


{-| Toggle subcommands
-}
type Toggle
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
            Just Shift

        17 ->
            Just Ctrl

        18 ->
            Just Alt

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
                Just Left

            38 ->
                Just Up

            39 ->
                Just Right

            40 ->
                Just Down

            _ ->
                Nothing
