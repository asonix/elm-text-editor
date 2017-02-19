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
module Paragraph exposing
    (Paragraph, empty, isEmpty, last, lastWithDefault, toStart, toEnd,
    currentStyle, currentStyleWithDefault, setCurrentStyle,
    selectPreviousStyle, selectNextStyle,
    hasPreviousStyle, hasNextStyle,
    updateCurrentStyle)

{-|
@docs Paragraph
@docs empty, isEmpty, last, lastWithDefault, toStart, toEnd
@docs currentStyle, currentStyleWithDefault setCurrentStyle
@docs selectPreviousStyle, selectNextStyle
@docs hasPreviousStyle, hasNextStyle
@docs updateParagraph
-}


import Styles exposing (Style)
import Util exposing (fromMaybe, fromMaybeWithDefault)
import MaybeZipList exposing (..)


{-| Paragraph is an alias for Maybe (List Style)
-}
type alias Paragraph = MaybeZipList Style


{-| empty returns an empty paragraph

examples:

  > empty
  Nothing

-}
empty : Paragraph
empty = MaybeZipList.empty


{-| return whether or not paragraph is empty

examples:

  > isEmpty (ZipList.fromList [Style.empty])
  False

  > isEmpty (ZipList.fromList [])
  True

-}
isEmpty : Paragraph -> Bool
isEmpty = MaybeZipList.isEmpty


{-| get the last style from the paragraph

examples:

  > last (ZipList.fromList [Style.empty])
  Just (Style.empty)

  > last (ZipList.fromList [])
  Nothing

-}
last : Paragraph -> Maybe Style
last = MaybeZipList.current << MaybeZipList.toEnd


{-| get the last style from the paragraph

if the paragraph is Nothing, return the default

examples:

  > lastWithDefault defaultStyle (ZipList.fromList [Style.empty])
  Style.empty

  > lastWithDefault defaultStyle (ZipList.fromList [])
  defaultStyle

-}
lastWithDefault : Style -> Paragraph -> Style
lastWithDefault = Util.fromMaybeWithDefault (last)


{-| Shift currentStyle to Start
-}
toStart : Paragraph -> Paragraph
toStart = MaybeZipList.toStart


{-| Shift currentStyle to End
-}
toEnd : Paragraph -> Paragraph
toEnd = MaybeZipList.toStart


{-| get the current style
-}
currentStyle : Paragraph -> Maybe Style
currentStyle = MaybeZipList.current


{-| get the current style with a default fallback
-}
currentStyleWithDefault : Style -> Paragraph -> Style
currentStyleWithDefault = Util.fromMaybeWithDefault (currentStyle)


{-| set the current style
-}
setCurrentStyle : Style -> Paragraph -> Paragraph
setCurrentStyle style = MaybeZipList.setCurrent (Just style)


{-| shift to previous style
-}
selectPreviousStyle : Paragraph -> Paragraph
selectPreviousStyle = MaybeZipList.shiftBack


{-| shift to next style
-}
selectNextStyle : Paragraph -> Paragraph
selectNextStyle = MaybeZipList.shiftForward


{-| checks if previous style exists
-}
hasPreviousStyle : Paragraph -> Bool
hasPreviousStyle = MaybeZipList.canShiftBack


{-| checks if next style exists
-}
hasNextStyle : Paragraph -> Bool
hasNextStyle = MaybeZipList.canShiftForward


{-| Applies update function to current style
-}
updateCurrentStyle : (Style -> Style) -> Paragraph -> Paragraph
updateCurrentStyle fn paragraph =
  paragraph
    |> currentStyle
    |> Util.fromMaybeWithDefault identity Style.empty
    |> fn
    |> flip (setCurrentStyle) paragraph
