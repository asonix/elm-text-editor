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
module MaybeZipList exposing
    (MaybeZipList, shiftBack, shiftForward, toStart, toEnd,
    insertBefore, insertAfter, removeCurrentForward, removeCurrentBack
    toList, fromList, current, setCurrent, empty, isEmpty,
    canShiftBack, canShiftForward)

{-|
@docs MaybeZipList
@docs shiftForward, shiftBack, toStart, toEnd
@docs insertBefore, insertAfter
@docs removeCurrentForward, removeCurrentBack
@docs toList, fromList
@docs current, setCurrent
@docs empty, isEmpty
@docs canShiftBack, canShiftForward
-}

{-| MaybeZipLists are useful for ensuring a list can only ever
have zero or one Nothing

They also have a concept of a "current" element.
The MaybeZipList's previous and next lists are treated as stacks.
-}
type MaybeZipList a =
  MaybeZipList
    { previous : List a
    , current : Maybe a
    , next : List a
    }


{- shiftBack cycles through the MaybeZipList towards the beginning

examples:

  > shiftBack (MaybeZipList {[3, 2, 1], Just 4, [5, 6, 7]})
  MaybeZipList {[2, 1], Just 3, [4, 5, 6, 7]}

  > shiftBack (MaybeZipList {[2, 1], Just 3, [4, 5, 6, 7]})
  MaybeZipList {[1], Just 2, [3, 4, 5, 6, 7]}

  > shiftBack (MaybeZipList {[1], Just 2, [3, 4, 5, 6, 7]})
  MaybeZipList {[], Just 1, [2, 3, 4, 5, 6, 7]}

  > shiftBack (MaybeZipList {[], Just 1, [2, 3, 4, 5, 6, 7]})
  MaybeZipList {[], Just 1, [2, 3, 4, 5, 6, 7]}

  > shiftBack (MaybeZipList {[3, 2, 1], Nothing, [5, 6, 7]})
  MaybeZipList {[2, 1], Just 3, [5, 6, 7]}

  > shiftBack (MaybeZipList {[], Nothing, [5, 6, 7]})
  MaybeZipList {[], Nothing, [5, 6, 7]}

-}
shiftBack : MaybeZipList a -> MaybeZipList a
shiftBack (MaybeZipList {previous, current, next}) =
  let
      new_next =
        case current of
          Just c ->
            c::next

          Nothing ->
            next
  in
      case previous of
        [] ->
          MaybeZipList
            { previous = []
            , current = current
            , next = next
            }

        [last] ->
          MaybeZipList
            { previous = []
            , current = Just last
            , next = new_next
            }

        first::rest ->
          MaybeZipList
            { previous = rest
            , current = Just first
            , next = new_next
            }


{- shiftForward cycles through the MaybeZipList towards the end

examples:

  > shiftForward (MaybeZipList {[3, 2, 1], Just 4, [5, 6, 7]})
  MaybeZipList {[4, 3, 2, 1], Just 5, [6, 7]}

  > shiftForward (MaybeZipList {[4, 3, 2, 1], Just 5, [6, 7]})
  MaybeZipList {[5, 4, 3, 2, 1], Just 6, [7]}

  > shiftForward (MaybeZipList {[5, 4, 3, 2, 1], Just 6, [7]})
  MaybeZipList {[6, 5, 4, 3, 2, 1], Just 7, []}

  > shiftForward (MaybeZipList {[6, 5, 4, 3, 2, 1], Just 7, []})
  MaybeZipList {[6, 5, 4, 3, 2, 1], Just 7, []}

  > shiftForward (MaybeZipList {[3, 2, 1], Nothing, [5, 6, 7]})
  MaybeZipList {[3, 2, 1], Nothing, [6, 7]}

  > shiftForward (MaybeZipList {[6, 5, 4, 3, 2, 1], Nothing, []})
  MaybeZipList {[6, 5, 4, 3, 2, 1], Nothing, []}

-}
shiftForward : MaybeZipList a -> MaybeZipList a
shiftForward (MaybeZipList {previous, current, next}) =
  let
      new_previous =
        case current of
          Just c ->
            c::previous

          Nothing ->
            previous
  in
      case next of
        [] ->
          MaybeZipList
            { previous = previous
            , current = current
            , next = []
            }

        [first] ->
          MaybeZipList
            { previous = new_previous
            , current = Just first
            , next = []
            }

        first::rest ->
          MaybeZipList
            { previous = new_previous
            , current = Just first
            , next = rest
            }


{-| toStart resets the MaybeZipList to the beginning

examples:

  toStart (MaybeZipList {[3, 2, 1], Just 4, [5, 6]})
  MaybeZipList {[], Just 1, [2, 3, 4, 5, 6]}

  toStart (MaybeZipList {[], Just 1, []})
  MaybeZipList {[], Just 1, []}

  toStart (MaybeZipList {[3, 2, 1], Nothing, [5, 6]})
  MaybeZipList {[], Just 1, [2, 3, 5, 6]}

  toStart (MaybeZipList {[], Nothing, []})
  MaybeZipList {[], Nothing, []}

-}
toStart : MaybeZipList a -> MaybeZipList a
toStart (MaybeZipList {previous, current, next}) =
  let
      new_next =
        case current of
          Just c ->
            c::next

          Nothing ->
            next
  in
      case previous of
        [] ->
          MaybeZipList
            { previous = previous
            , current = current
            , next = next
            }

        [first] ->
          MaybeZipList
            { previous = []
            , current = Just first
            , next = new_next
            }

        first::rest ->
          toStart
            (MaybeZipList
            { previous = rest
            , current = Just first
            , next = new_next
            })


{-| toEnd resets the MaybeZipList to the beginning

examples:

  toEnd (MaybeZipList {[3, 2, 1], 4, [5, 6]})
  MaybeZipList {[5, 4, 3, 2, 1], 6, []}

  toEnd (MaybeZipList {[], 1, []})
  MaybeZipList {[], 1, []}

  toEnd (MaybeZipList {[3, 2, 1], Nothing, [5, 6]})
  MaybeZipList {[5, 3, 2, 1], 6, []}

  toEnd (MaybeZipList {[], Nothing, []})
  MaybeZipList {[], Nothing, []}

-}
toEnd : MaybeZipList a -> MaybeZipList a
toEnd (MaybeZipList {previous, current, next}) =
  let
      new_previous =
        case current of
          Just c ->
            c::previous

          Nothing ->
            previous
  in
      case next of
        [] ->
          MaybeZipList
            { previous = previous
            , current = current
            , next = next
            }

        [first] ->
          MaybeZipList
            { previous = new_previous
            , current = Just first
            , next = []
            }

        first::rest ->
          toEnd
            (MaybeZipList
            { previous = new_previous
            , current = Just first
            , next = rest
            })


{-| insertBefore puts a new item in the MaybeZipList before the current item

examples:

  > insertBefore (Just 3) (MaybeZipList {[2, 1], Just 4, [5, 6]})
  MaybeZipList {[2, 1], 3, [4, 5, 6]}

  > insertBefore (Just 3) (MaybeZipList {[], Just 1, []})
  MaybeZipList {[], Just 3, [1]}

  > insertBefore Nothing (MaybeZipList {[], Just 3, []})
  MaybeZipList {[], Nothing, [3]}

  > insertBefore Nothing (MaybeZipList {[], Nothing, []})
  MaybeZipList {[], Nothing, []}

-}
insertBefore : Maybe a -> MaybeZipList a -> MaybeZipList a
insertBefore elem (MaybeZipList {previous, current, next}) =
  let
      new_next =
        case current of
          Just c ->
            c::next

          Nothing ->
            next
  in
      MaybeZipList
        { previous = previous
        , current = elem
        , next = new_next
        }


{-| insertAfter puts a new item in the MaybeZipList before the current item

examples:

  > insertAfter (Just 3) (MaybeZipList {[2, 1], Just 4, [5, 6]})
  MaybeZipList {[4, 2, 1], 3, [5, 6]}

  > insertAfter (Just 3) (MaybeZipList {[], Just 1, []})
  MaybeZipList {[1], Just 3, []}

  > insertAfter Nothing (MaybeZipList {[2, 1], Just 4, [5, 6]})
  MaybeZipList {[4, 2, 1], Nothing, [5, 6]}

  > insertAfter Nothing (MaybeZipList {[], Nothing, []})
  MaybeZipList {[], Nothing, []}

-}
insertAfter : Maybe a -> MaybeZipList a -> MaybeZipList a
insertAfter elem (MaybeZipList {previous, current, next}) =
  let
      new_previous =
        case current of
          Just c ->
            c::previous

          Nothing ->
            previous
  in
      MaybeZipList
        { previous = new_previous
        , current = elem
        , next = next
        }


{-| removeCurrentForward shifts the top of the next stack into current

examples:

  > removeCurrentForward (MaybeZipList {[3, 2, 1], 4, [5, 6, 7]})
  MaybeZipList {[3, 2, 1], 5, [6, 7]}

  > removeCurrentForward (MaybeZipList {[3, 2, 1], 5, [6, 7]})
  MaybeZipList {[3, 2, 1], 6, [7]}

  > removeCurrentForward (MaybeZipList {[3, 2, 1], 6, [7]})
  MaybeZipList {[3, 2, 1], 7, []}

  > removeCurrentForward (MaybeZipList {[3, 2, 1], 7, []})
  MaybeZipList {[3, 2, 1], 7, []}

-}
removeCurrentForward : MaybeZipList a -> MaybeZipList a
removeCurrentForward (MaybeZipList {previous, current, next}) =
  case next of
    [] ->
      MaybeZipList
        { previous = previous
        , current = current
        , next = next
        }

    [first] ->
      MaybeZipList
        { previous = previous
        , current = Just first
        , next = []
        }

    first::rest ->
      MaybeZipList
        { previous = previous
        , current = Just first
        , next = rest
        }


{-| removeCurrentBack shifts the top of the previous stack into current

examples:

  > removeCurrentBack (MaybeZipList {[3, 2, 1], Just 4, [5, 6, 7]})
  MaybeZipList {[2, 1], Just 3, [5, 6, 7]}

  > removeCurrentBack (MaybeZipList {[2, 1], Just 3, [5, 6, 7]})
  MaybeZipList {[1], Just 2, [5, 6, 7]}

  > removeCurrentBack (MaybeZipList {[1], Just 2, [5, 6, 7]})
  MaybeZipList {[], Just 1, [5, 6, 7]}

  > removeCurrentBack (MaybeZipList {[], Just 1, [5, 6, 7]})
  MaybeZipList {[], Just 1, [5, 6, 7]}

  > removeCurrentBack (MaybeZipList {[3, 2, 1], Nothing, [5, 6, 7]})
  MaybeZipList {[2, 1], Just 3, [5, 6, 7]}

  > removeCurrentBack (MaybeZipList {[], Nothing, [5, 6, 7]})
  MaybeZipList {[], Nothing, [5, 6, 7]}

-}
removeCurrentBack : MaybeZipList a -> MaybeZipList a
removeCurrentBack (MaybeZipList {previous, current, next}) =
  case previous of
    [] ->
      MaybeZipList
        { previous = previous
        , current = current
        , next = next
        }

    [first] ->
      MaybeZipList
        { previous = []
        , current = Just first
        , next = next
        }

    first::rest ->
      MaybeZipList
        { previous = rest
        , current = Just first
        , next = next
        }


{-| toList converts a MaybeZipList into a List

examples:

  > toList (MaybeZipList {[3, 2, 1], 4, [5, 6, 7]})
  [1, 2, 3, 4, 5, 6, 7]

  > toList (MaybeZipList {[], 1, []})
  [1]

  > toList (MaybeZipList {[3, 2, 1], Nothing, [5, 6, 7]})
  [1, 2, 3, 5, 6, 7]

  > toList (MaybeZipList {[], Nothing, []})
  []

-}
toList : MaybeZipList a -> List a
toList (MaybeZipList {previous, current, next}) =
  let
      new_next =
        case current of
          Just c ->
            c::next

          Nothing ->
            next
  in
      (List.reverse previous) ++ new_next


{-| fromList converts a List into a MaybeZipList

examples:

  > fromList [1, 2, 3, 4, 5]
  MaybeZipList {[], 1, [2, 3, 4, 5]}

  > fromList []
  MaybeZipList {[], Nothing, []}

-}
fromList : List a -> MaybeZipList a
fromList list =
  case list of
    [] ->
      MaybeZipList
        { previous = []
        , current = Nothing
        , next = []
        }

    [first] ->
      MaybeZipList
        { previous = []
        , current = Just first
        , next = []
        }

    first::rest ->
      MaybeZipList
        { previous = []
        , current = Just first
        , next = rest
        }


{-| Get the current element from the MaybeZipList

examples:

  > current (MaybeZipList {[3, 2, 1], 4, [5, 6, 7]})
  4

  > current (MaybeZipList {[], 1, []})
  1

-}
current : MaybeZipList a -> Maybe a
current (MaybeZipList {current}) = current


{-| Sets the current value

examples:

  > empty |> setCurrent (Just 5)
  MaybeZipList {[], Just 5, []}

-}
setCurrent : Maybe a -> MaybeZipList a -> MaybeZipList a
setCurrent elem (MaybeZipList {previous, current, next}) =
  MaybeZipList
    { previous = previous
    , current = elem
    , next = next
    }


{-| Create a new, empty MaybeZipList
-}
empty : MaybeZipList a
empty =
  MaybeZipList
    { previous = []
    , current = Nothing
    , next = []
    }


{-| Check if MaybeZipList is empty

examples:

  > isEmpty empty
  True

  > isEmpty (empty |> setCurrent (Just 5))
  False

-}
isEmpty : MaybeZipList a -> Bool
isEmpty (MaybeZipList {previous, current, next}) =
  List.isEmpty previous && List.isEmpty next && current == Nothing


{-| Returns true if current isn't at the beginning
-}
canShiftBack : MaybeZipList a -> Bool
canShiftBack (MaybeZipList {previous}) =
  not (List.isEmpty previous)


{-| Returns true if current isn't at the end
-}
canShiftForward : MaybeZipList a -> Bool
canShiftForward (MaybeZipList {next}) =
  not (List.isEmpty next)
