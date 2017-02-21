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


module Util exposing (fromMaybe, fromMaybeWithDefault, fromTwoMaybes, fromTwoMaybesWithDefault)

{-|
@docs fromMaybe, fromMaybeWithDefault
@docs fromTwoMaybes, fromTwoMaybesWithDefault
-}


{-| fromMaybe allows the chaining of uncertain operations

examples:

  > [1, 2, 3] |> List.tail |> fromMaybe (List.head)
  Just 2

  > [1] |> List.tail |> fromMaybe (List.head)
  Nothing

-}
fromMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
fromMaybe fn =
    fromMaybeWithDefault fn Nothing


{-| fromMaybeWithDefault allows a value to be resolved from uncertain operations


examples:

  > [1, 2, 3] |> List.tail |> fromMaybeWithDefault (List.head) 0
  2

  > [1] |> List.tail |> fromMaybeWithDefault (List.head) 0
  0

-}
fromMaybeWithDefault : (a -> b) -> b -> Maybe a -> b
fromMaybeWithDefault fn default maybe_item =
    case maybe_item of
        Just item ->
            fn item

        Nothing ->
            default


{-| fromTwoMaybes allows composition of multiple maybe types

examples:

  > fromTwoMaybes (identity) (Just "one") (Just "two")
  Just ("one", "two")

  > fromTwoMaybes (identity) (Just "one") (Nothing)
  Nothing

-}
fromTwoMaybes : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
fromTwoMaybes fn =
    fromTwoMaybesWithDefault fn Nothing


{-| fromTwoMaybesWithDefault allows a value to be resolved from two uncertain values

examples:

  > fromTwoMaybesWithDefault (curry identity) (0, "four") (Just 1) (Just "two")
  (1, "two")

  > fromTwoMaybesWithDefault (curry identity) (0, "four") (Just 1) (Nothing)
  (0, "four")

-}
fromTwoMaybesWithDefault : (a -> b -> c) -> c -> Maybe a -> Maybe b -> c
fromTwoMaybesWithDefault fn default maybe_one maybe_two =
    fromMaybeWithDefault
        (\x -> fromMaybeWithDefault (fn x) default maybe_two)
        default
        maybe_one
