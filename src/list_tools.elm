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
module ListTools exposing
    (last, lastWithDefault, headWithDefault, dropLast, dropHead)

{-|
@docs last, lastWithDefault, headWithDefault, dropLast, dropHead
-}


import Util exposing (fromMaybeWithDefault)


{-| last gets the last item in the list

examples:

  > last [1, 2, 3, 4]
  Just 4

  > last []
  Nothing

-}
last : List a -> Maybe a
last = List.head << List.reverse


{-| lastWithDefault gets the last item in the list

if the list is empty, we return the default value

examples:

  > lastWithDefault 0 [1, 2, 3, 4]
  4

  > lastWithDefault 0 []
  0

-}
lastWithDefault : a -> List a -> a
lastWithDefault default list =
  list
    |> last
    |> Util.fromMaybeWithDefault identity default


{-| headWithDefault gets the first item in the list

if the list is empty, we return the default value

examples:

  > headWithDefault 0 [1, 2, 3, 4]
  1

  > headWithDefault 0 []
  0

-}
headWithDefault : a -> List a -> a
headWithDefault default list =
  list
    |> List.head
    |> Util.fromMaybeWithDefault identity default


{-| dropLast removes the last item from the list

examples:

  > dropLast [1, 2, 3, 4]
  [1, 2, 3]

  > dropLast []
  []

-}
dropLast : List a -> List a
dropLast list =
  List.take (List.length list - 1) list


{-| dropHead removes the first item from the list

examples:

  > dropHead [1, 2, 3]
  [2, 3]

  > dropHead []
  []

-}
dropHead : List a -> List a
dropHead = drop 1
