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
module Content exposing
    (Content, init, currentParagraph, setCurrentParagraph,
    selectNextParagraph, selectPreviousParagraph,
    hasNextParagraph, hasPreviousParagraph,
    updateCurrentParagraph, mergeWithPreviousParagraph,
    insertParagraph, toList)

{-|
@docs Content
@docs init, currentParagraph, setCurrentParagraph,
@docs selectNextParagraph, selectPreviousParagraph
@docs hasNextParagraph, hasPreviousParagraph
@docs updateCurrentParagraph
@docs mergeWithPreviousParagraph
@docs insertParagraph
@docs toList
-}


import Paragraph exposing (..)
import ZipList exposing (..)


{-| Content is an alias for List Paragraph
-}
type alias Content = ZipList Paragraph


{-| init returns a new content with an empty paragraph inside
-}
init : Content
init = ZipList.init Paragraph.empty


{-| Get the current paragraph
-}
currentParagraph : Content -> Paragraph
currentParagraph = ZipList.current


{-| Set the current paragraph
-}
setCurrentParagraph : Paragraph -> Content -> Content
setCurrentParagraph = ZipList.setCurrent


{-| Gets the next paragraph
-}
selectNextParagraph : Content -> Content
selectNextParagraph = ZipList.shiftForward


{-| Gets the previous paragraph
-}
selectPreviousParagraph : Content -> Content
selectPreviousParagraph = ZipList.shiftBack


{-| Checks if next paragraph exists
-}
hasNextParagraph : Content -> Bool
hasNextParagraph = ZipList.canShiftForward


{-| Checks if previous paragraph exists
-}
hasPreviousParagraph : Content -> Bool
hasPreviousParagraph = ZipList.canShiftBack


{-| Applies update function to current paragraph
-}
updateCurrentParagraph : (Paragraph -> Paragraph) -> Content -> Content
updateCurrentParagraph fn content =
  content
    |> currentParagraph
    |> fn
    |> flip (setCurrent) content


{-| Merge the current and previous paragraphs
-}
mergeWithPreviousParagraph : Content -> Content
mergeWithPreviousParagraph content =
  let
      p1 : Paragraph
      p1 =
        content
          |> selectPreviousParagraph
          |> currentParagraph

      p2 : Paragraph
      p2 =
        content
          |> currentParagraph

      merged : Paragraph
      merged =
        Paragraph.merge p1 p2
  in
      content
        |> ZipList.removeCurrentBack
        |> setCurrentParagraph merged


{-| Insert a new paragraph after the current paragraph and select it
-}
insertParagraph : List Style -> Content -> Content
insertParagraph = ZipList.insertAfter << Paragraph.fromList


{-| Make a List of Paragraphs from a Content
-}
toList : Content -> List Paragraph
toList = ZipList.toList
