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
module Keys exposing
    (KeyCmd(..), Modifiers, handleCode)

{-|
@docs KeyCmd

@docs handleCode
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
  | NewLine
  | NewParagraph
  | ToggleCode
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
  { ctrl: Bool
  , alt: Bool
  , shift: Bool
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
      66 -> -- B
        Just ToggleBold

      67 -> -- C
        Just ToggleCode

      72 -> -- H
        Just ToggleHeading

      73 -> -- I
        Just ToggleItalic

      76 -> -- L
        Just ToggleLink

      80 -> -- I
        Just ToggleImage

      83 -> -- S
        Just ToggleStrike

      85 -> -- U
        Just ToggleUnderline

      _ ->
        Nothing

  else
    case key_code of
      13 -> -- Enter
        Just (if modifiers.shift then NewLine else NewParagraph)

      _ ->
        Nothing
