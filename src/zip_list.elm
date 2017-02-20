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
module ZipList exposing
    (ZipList, shiftBack, shiftForward, toStart, toEnd,
    insertBefore, insertAfter, removeCurrentForward, removeCurrentBack,
    toList, fromList, current, setCurrent, init,
    canShiftBack, canShiftForward, merge,
    serializeToString)

{-|
@docs ZipList
@docs shiftForward, shiftBack, toStart, toEnd
@docs insertBefore, insertAfter
@docs removeCurrentForward, removeCurrentBack
@docs toList, fromList
@docs current, setcurrent
@docs init
@docs canShiftBack, canShiftForward
@docs merge
@docs serializeToString
-}


import MaybeZipList exposing (..)


{-| ZipLists are useful for ensuring a list can never be empty

This ZipList is a wrapper around MaybeZipList, and includes a default value
-}
type ZipList a =
  ZipList
    { maybe_zip_list : MaybeZipList a
    , default_value : a
    }


{- shiftBack cycles through the ZipList towards the beginning
-}
shiftBack : ZipList a -> ZipList a
shiftBack (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.shiftBack maybe_zip_list
    , default_value = default_value
    }


{- shiftForward cycles through the ZipList towards the end
-}
shiftForward : ZipList a -> ZipList a
shiftForward (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.shiftForward maybe_zip_list
    , default_value = default_value
    }


{-| toStart resets the ZipList to the beginning
-}
toStart : ZipList a -> ZipList a
toStart (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.toStart maybe_zip_list
    , default_value = default_value
    }


{-| toEnd resets the ZipList to the beginning
-}
toEnd : ZipList a -> ZipList a
toEnd (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.toEnd maybe_zip_list
    , default_value = default_value
    }


{-| insertBefore puts a new item in the ZipList before the current item
-}
insertBefore : a -> ZipList a -> ZipList a
insertBefore elem (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.insertBefore (Just elem) maybe_zip_list
    , default_value = default_value
    }


{-| insertAfter puts a new item in the ZipList before the current item
-}
insertAfter : a -> ZipList a -> ZipList a
insertAfter elem (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.insertAfter (Just elem) maybe_zip_list
    , default_value = default_value
    }



{-| removeCurrentForward shifts the top of the next stack into current
-}
removeCurrentForward : ZipList a -> ZipList a
removeCurrentForward (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.removeCurrentForward maybe_zip_list
    , default_value = default_value
    }


{-| removeCurrentBack shifts the top of the previous stack into current
-}
removeCurrentBack : ZipList a -> ZipList a
removeCurrentBack (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.removeCurrentBack maybe_zip_list
    , default_value = default_value
    }


{-| toList converts a ZipList into a List
-}
toList : ZipList a -> List a
toList (ZipList {maybe_zip_list}) =
  MaybeZipList.toList maybe_zip_list


{-| fromList converts a List into a ZipList
-}
fromList : a -> List a -> ZipList a
fromList default list =
  ZipList
    { maybe_zip_list = MaybeZipList.fromList list
    , default_value = default
    }


{-| Get the current element from the ZipList
-}
current : ZipList a -> a
current (ZipList {default_value, maybe_zip_list}) =
  case MaybeZipList.current maybe_zip_list of
    Just c ->
      c

    Nothing ->
      default_value


{-| Sets the current value
-}
setCurrent : a -> ZipList a -> ZipList a
setCurrent elem (ZipList {default_value, maybe_zip_list}) =
  ZipList
    { maybe_zip_list = MaybeZipList.setCurrent (Just elem) maybe_zip_list
    , default_value = default_value
    }


{-| Create a new ZipList with one element
-}
init : a -> ZipList a
init default =
  ZipList
    { maybe_zip_list = MaybeZipList.fromList [default]
    , default_value = default
    }


{-| Returns true if current isn't at the beginning
-}
canShiftBack : ZipList a -> Bool
canShiftBack (ZipList {maybe_zip_list}) =
  MaybeZipList.canShiftBack maybe_zip_list


{-| Returns true if current isn't at the end
-}
canShiftForward : ZipList a -> Bool
canShiftForward (ZipList {maybe_zip_list}) =
  MaybeZipList.canShiftForward maybe_zip_list


{-| Merges current ZipList with another
-}
merge : ZipList a -> ZipList a -> ZipList a
merge (ZipList {default_value, maybe_zip_list}) (ZipList two) =
  ZipList
    { maybe_zip_list = MaybeZipList.merge maybe_zip_list two.maybe_zip_list
    , default_value = default_value
    }


{-| Serialize the current item to String
-}
serializeToString : (a -> String) -> ZipList a -> String
serializeToString fn (ZipList {default_value, maybe_zip_list}) =
  let
      string_default_value : String
      string_default_value =
        "default_value: " ++ (fn default_value)

      string_maybe_zip_list : String
      string_maybe_zip_list =
        "maybe_zip_list: ("
          ++ (MaybeZipList.serializeToString fn maybe_zip_list)
          ++ ")"
  in
      "ZipList {"
        ++ string_default_value ++ ", "
        ++ string_maybe_zip_list ++ "}"
