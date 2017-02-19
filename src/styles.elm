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
module Styles exposing
    (Style, setLinkHref, setText, updateText, appendText, toggleCode,
    toggleImage, toggleText, toggleStrike, toggleUnderline, toggleItalic,
    toggleBold, toggleHeading, toggleLink, serializeToString, render,
    isEmpty, getText, setMouseoverText, empty)

{-|
@docs Style

@docs setLinkHref, setText, appendText, toggleCode, toggleImage, toggleStrike
@docs toggleUnderline, toggleItalic, toggleBold, toggleHeading, toggleLink
@docs serializeToString, render, isEmpty, getText, setMouseoverText
@docs toggleText, empty
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import String


type alias StyleAttributes
  = { heading : Bool
    , bold : Bool
    , italic : Bool
    , underline : Bool
    , strike : Bool
    }

type StyleType
  = Text StyleAttributes
  | Code
  | Image (Maybe String) -- Image with mouseover text

type LinkWrapper
  = Only StyleType
  | Link String StyleType

{-| Internal representation of Styled Text.
-}
type Style
  = Style
    { link_wrapper : LinkWrapper
    , text : String
    }


defaultAttributes : StyleAttributes
defaultAttributes =
  { heading = False
  , bold = False
  , italic = False
  , underline = False
  , strike = False
  }


{-| Initialize style with sane defaults
-}
empty : Style
empty =
  Style
    { link_wrapper = Only (Text defaultAttributes)
    , text = ""
    }


{-| Set the URL of a link in a content chain

examples:

  > appendText " hey" (Style {_, "hello"})
  Style {_, "hello hey"}

-}
appendText : String -> Style -> Style
appendText new_text (Style { link_wrapper, text }) =
  Style
    { link_wrapper = link_wrapper
    , text = text ++ new_text
    }


{-| Set the URL of a link in a content chain

examples:

  > setText "hello" (Style { _, "hi" })
  Style { _, "hello" }

-}
setText : String -> Style -> Style
setText text (Style { link_wrapper }) =
  Style
    { link_wrapper = link_wrapper
    , text = text
    }


{-| Apply a function to modify the style's text

examples :

  > updateText (dropRight 1) (Style { _, "hello world" })
  Style { _, "hello worl" }

-}
updateText : (String -> String) -> Style -> Style
updateText fn (Style { link_wrapper, text }) =
  Style
    { link_wrapper = link_wrapper
    , text = fn text
    }


{-| getText returns the string from inside the content

examples:

  > getText (Style { _, "hi" })
  "hi"

-}
getText : Style -> String
getText (Style { text }) =
  text


{-| Determines whether there is any text in the content

examples:

  > isEmpty (Style { _, "hello" })
  False

  > isEmpty (Style { _, "" })
  True

-}
isEmpty : Style -> Bool
isEmpty (Style { text }) =
  String.isEmpty text


{-| Set the URL of a link in a content chain

examples:

  > setLinkHref "https://google.com" (Style {Only _, "hello"})
  Style {Only _, "hello"}

  > setLinkHref "https://google.com" (Style {Link "https://yahoo.com" _, "hello"})
  Style {Link "https://google.com" _, "hello"}

-}
setLinkHref : String -> Style -> Style
setLinkHref href (Style { link_wrapper, text }) =
  case link_wrapper of
    Only _ ->
      Style
        { link_wrapper = link_wrapper
        , text = text
        }

    Link _ style_type ->
      Style
        { link_wrapper = Link href style_type
        , text = text
        }


{-| Links can be applied to any type of content.

examples:

  > toggleLink "https://google.com" (Style {Text _, "hello world"})
  Style {Link "https://google.com" _, "hello world"}

  > toggleLink "" Style {Link "https://google.com" _, "hello world"}
  Style {Text _, "hello world"}

-}
toggleLink : String -> Style -> Style
toggleLink href (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      Style
        { link_wrapper = Link href style_type
        , text = text
        }

    Link _ style_type ->
      Style
        { link_wrapper = Only style_type
        , text = text
        }


setMouseoverTextOfStyleType : String -> StyleType -> StyleType
setMouseoverTextOfStyleType string style_type =
  case style_type of
    Image _ ->
      Image (Just string)

    _ ->
      style_type


{-| Set the mouseover text of an image

examples:

  > setMouseoverText "A cute catte" (Style { Only (Image Nothing), "https://catte.com/cat.png" })
  Style { Only (Image (Just "A cute catte")), "https://catte.com/cat.png" })

  > setMouseoverText "some text" (Style { Only Code, "Enum.map/2" })
  Style { Only Code, "Enum.map/2" }
-}
setMouseoverText : String -> Style -> Style
setMouseoverText string (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      Style
        { link_wrapper = Only (setMouseoverTextOfStyleType string style_type)
        , text = text
        }

    Link href style_type ->
      Style
        { link_wrapper = Link href (setMouseoverTextOfStyleType string style_type)
        , text = text
        }


{-| These next functions are for toggling StyleAttributes.

They are useful within this module only
-}

toggleHeadingAttribute : StyleAttributes -> StyleAttributes
toggleHeadingAttribute style_attributes =
    { style_attributes | heading = not style_attributes.heading }


toggleBoldAttribute : StyleAttributes -> StyleAttributes
toggleBoldAttribute style_attributes =
    { style_attributes | bold = not style_attributes.bold }


toggleItalicAttribute : StyleAttributes -> StyleAttributes
toggleItalicAttribute style_attributes =
    { style_attributes | italic = not style_attributes.italic }


toggleUnderlineAttribute : StyleAttributes -> StyleAttributes
toggleUnderlineAttribute style_attributes =
    { style_attributes | underline = not style_attributes.underline }


toggleStrikeAttribute : StyleAttributes -> StyleAttributes
toggleStrikeAttribute style_attributes =
    { style_attributes | strike = not style_attributes.strike }


{- Attributes can only be toggled for Text
-}
toggleAttribute : (StyleAttributes -> StyleAttributes) -> StyleType -> StyleType
toggleAttribute fn style_type =
  case style_type of
    Text style_attributes ->
      Text (fn style_attributes)

    _ ->
      style_type


toggleAttributeFromStyle : (StyleAttributes -> StyleAttributes) -> Style -> Style
toggleAttributeFromStyle fn (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      Style
        { link_wrapper = Only (toggleAttribute fn style_type)
        , text = text
        }

    Link href style_type ->
      Style
        { link_wrapper =  Link href (toggleAttribute fn style_type)
        , text = text
        }


{-| Headings cannot be applied to Code or Image.

examples:

  > toggleHeading (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {False, True, True, True, True}), "Hello World" }

  > toggleHeading (Style { Only (Text {False, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleHeading (Style { Only Code, "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleHeading (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Image Nothing), "https://google.com/google.png" }

-}
toggleHeading : Style -> Style
toggleHeading =
  toggleAttributeFromStyle toggleHeadingAttribute


{-| Bold cannot be applied to Code or Image.

examples:

  > toggleBold (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, False, True, True, True}), "Hello World" }

  > toggleBold (Style { Only (Text {True, False, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleBold (Style { Only Code, "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleBold (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Image Nothing), "https://google.com/google.png" }

-}
toggleBold : Style -> Style
toggleBold =
  toggleAttributeFromStyle toggleBoldAttribute


{-| Italic cannot be applied to Code or Image.

examples:

  > toggleItalic (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, False, True, True}), "Hello World" }

  > toggleItalic (Style { Only (Text {True, True, False, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleItalic (Style { Only Code, "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleItalic (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Image Nothing), "https://google.com/google.png" }

-}
toggleItalic : Style -> Style
toggleItalic =
  toggleAttributeFromStyle toggleItalicAttribute


{-| Underline cannot be applied to Code or Image.

examples:

  > toggleUnderline (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, False, True}), "Hello World" }

  > toggleUnderline (Style { Only (Text {True, True, True, False, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleUnderline (Style { Only Code, "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleUnderline (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Image Nothing), "https://google.com/google.png" }

-}
toggleUnderline : Style -> Style
toggleUnderline =
  toggleAttributeFromStyle toggleUnderlineAttribute


{-| Strike cannot be applied to Code or Image.

examples:

  > toggleStrike (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, False}), "Hello World" }

  > toggleStrike (Style { Only (Text {True, True, True, True, False}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleStrike (Style { Only Code, "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleStrike (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Image Nothing), "https://google.com/google.png" }

-}
toggleStrike : Style -> Style
toggleStrike =
  toggleAttributeFromStyle toggleStrikeAttribute


{- The following section is for toggling StyleTypes to and from Text -}

toggleImageFromStyleType : StyleType -> StyleType
toggleImageFromStyleType style_type =
  case style_type of
    Image _ ->
      Text defaultAttributes

    _ ->
      Image Nothing


toggleCodeFromStyleType : StyleType -> StyleType
toggleCodeFromStyleType style_type =
  case style_type of
    Code ->
      Text defaultAttributes

    _ ->
      Code


toggleTextFromStyleType : StyleType -> StyleType
toggleTextFromStyleType style_type =
  case style_type of
    Text attributes ->
      style_type

    _ ->
      Text defaultAttributes


toggleStyleTypeFromStyle : (StyleType -> StyleType) -> Style -> Style
toggleStyleTypeFromStyle fn (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      Style
        { link_wrapper = Only (fn style_type)
        , text = text
        }

    Link href style_type ->
      Style
        { link_wrapper = Link href (fn style_type)
        , text = text
        }


{-| toggleImage removes styling (except link) and converts the remaining Text to an Image

examples:

  > toggleImage (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Image Nothing), "Hello World" }

  > toggleImage (Style { Only Code, "Hello World" })
  Style { Only (Image Nothing), "Hello World" }

  > toggleImage (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Text {False, False, False, False, False}), "https://google.com/google.png" }

-}
toggleImage : Style -> Style
toggleImage =
  toggleStyleTypeFromStyle toggleImageFromStyleType


{-| toggleCode removes styling (except link) and converts the remaining Text to Code

examples:

  > toggleCode (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only Code, "Hello World" }

  > toggleCode (Style { Only Code, "Hello World" })
  Style { Only (Text {False, False, False, False, False}), "Hello World" }

  > toggleCode (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only Code, "https://google.com/google.png" }

-}
toggleCode : Style -> Style
toggleCode =
  toggleStyleTypeFromStyle toggleCodeFromStyleType


{-| toggleText ensures the style is of Text type

examples:

  > toggleText (Style { Only (Text {True, True, True, True, True}), "Hello World" })
  Style { Only (Text {True, True, True, True, True}), "Hello World" }

  > toggleText (Style { Only Code, "Hello World" })
  Style { Only (Text {False, False, False, False, False}), "Hello World" }

  > toggleText (Style { Only (Image Nothing), "https://google.com/google.png" })
  Style { Only (Text {False, False, False, False, False}), "https://google.com/google.png" }

-}
toggleText : Style -> Style
toggleText =
  toggleStyleTypeFromStyle toggleTextFromStyleType


serializeStyleAttributesToString : StyleAttributes -> String
serializeStyleAttributesToString style_attributes =
  let
      heading = "heading: "
        ++ if style_attributes.heading then "True" else "False"

      bold = "bold: "
        ++ if style_attributes.bold then "True" else "False"

      italic = "italic: "
        ++ if style_attributes.italic then "True" else "False"

      underline = "undelrine: "
        ++ if style_attributes.underline then "True" else "False"

      strike = "strike: "
        ++ if style_attributes.strike then "True" else "False"
  in
    heading ++ ", " ++ bold ++ ", " ++ italic ++ ", "
      ++ underline ++ ", " ++ strike


serializeStyleTypeToString : StyleType -> String
serializeStyleTypeToString style_type =
  case style_type of
    Text attributes ->
      "Text (" ++ (serializeStyleAttributesToString attributes) ++ ")"

    Image mouseover ->
      case mouseover of
        Just text ->
          "Image (Just " ++ text ++ ")"

        Nothing ->
          "Image Nothing"

    Code ->
      "Code"

{-| Provide a string representation of styling

This is for debugging use only
-}
serializeToString : Style -> String
serializeToString (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      "Style { Only (" ++ (serializeStyleTypeToString style_type) ++ "), " ++ text ++ " }"

    Link href style_type ->
      "Style { Link " ++ href ++ " (" ++ (serializeStyleTypeToString style_type) ++ "), " ++ text ++ " }"


renderAttributes : StyleAttributes -> String -> Html msg
renderAttributes attributes str =
  let
      heading =
        if attributes.heading then
          h3 [] [ text str ]
        else
          text str

      bold =
        if attributes.bold then
          b [] [ heading ]
        else
          heading

      italic =
        if attributes.italic then
          i [] [ bold ]
        else
          bold

      underline =
        if attributes.underline then
          u [] [ italic ]
        else
          italic

      strike =
        if attributes.strike then
          span [ class "strike" ] [ underline ]
        else
          underline
  in
      strike

renderType : StyleType -> String -> Html msg
renderType style_type string =
  case style_type of
    Text attributes ->
      renderAttributes attributes string

    Image mouseover ->
      case mouseover of
        Just str ->
          img [ src string, title str ] []

        Nothing ->
          img [ src string ] []

    Code ->
      code [] [ text string ]

{-| Converts a Style into HTML elements

Dom Nodes galore.

This could be refactored to apply classes to an external span with the addition
of a helper function

examples:

  > render (Style { Only (Text {False, True, False, False, False}), "hello world" })
  b [] [ text "hello world" ]

  > render (Style { Link "https://google.com" (Text {True, True, True, True, True}), "hello world" })
  a [ href "https://google.com" ]
    [ h3 []
        [ b []
            [ i []
                [ u []
                    [ span [ class "strike" ]
                        [ text "hello world" ]
                    ]
                ]
            ]
        ]
    ]
-}
render : Style -> Html msg
render (Style { link_wrapper, text }) =
  case link_wrapper of
    Only style_type ->
      renderType style_type text

    Link url style_type ->
      a [ href url ] [ renderType style_type text ]
