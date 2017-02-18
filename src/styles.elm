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
    (Style, setLinkHref, setText, appendText, toggleCode, toggleImage,
    toggleText, toggleStrike, toggleUnderline, toggleItalic, toggleBold,
    toggleHeading, toggleLink, serializeToString, renderStyle, isEmpty,
    getText, setMouseoverText)

{-|
@docs Style

@docs setLinkHref, setText, appendText, toggleCode, toggleImage, toggleStrike
@docs toggleUnderline, toggleItalic, toggleBold, toggleHeading, toggleLink
@docs serializeToString, renderStyle, isEmpty, getText, setMouseoverText
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import String


{-| Internal representation of Styled Text.
-}

type StyleAttributes
  = StyleAttributes
    { heading : Bool
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
  = Just StyleType
  | Link String StyleType

type Style
  = Style
    { style_type : LinkWrapper
    , text : String
    }


defaultAttributes : StyleAttributes
defaultAttributes =
  StyleAttributes
    { heading = False
    , bold = False
    , italic = False
    , underline = False
    , strike = False
    }


{-| Set the URL of a link in a content chain

examples:

  > appendText " hey" (Style {_, "hello"})
  Style {_, "hello hey"}

-}
appendText : String -> Style -> Style
appendText text (Style { link_wrapper, styled_text } =
  Style { link_wrapper, styled_text ++ text }


{-| Set the URL of a link in a content chain

examples:

  > setText "hello" (Style { _, "hi" })
  Style { _, "hello" }

-}
setText : String -> Style -> Style
setText text (Style { link_wrapper, _ } =
  Style { link_wrapper, text }


{-| getText returns the string from inside the content

examples:

  > getText (Style { _, "hi" })
  "hi"

-}
getText : Style -> String
getText (Style { _, text }) = text


{-| Determines whether there is any text in the content

examples:

  > isEmpty (Style { _, "hello" })
  False

  > isEmpty (Style { _, "" })
  True

-}
isEmpty : Style -> Bool
isEmpty (Style { _, text }) = String.isEmpty text


{-| Set the URL of a link in a content chain

examples:

  > setLinkHref "https://google.com" (Style {Just _, "hello"})
  Style {Just _, "hello"}

  > setLinkHref "https://google.com" (Style {Link "https://yahoo.com" _, "hello"})
  Style {Link "https://google.com" _, "hello"}

-}
setLinkHref : String -> Style -> Style
setLinkHref href (Style { link_wrapper, text } =
  case link_wrapper of
    Just _ ->
      Style { link_wrapper, text }

    Link _ style_type ->
      Style { Link href style_type, text }


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
    Just style_type ->
      Style { Link href style_type, text }

    Link _ style_type ->
      Style { Just style_type, text }


setMouseoverTextOfStyleType : String -> StyleType -> StyleType
setMouseoverTextOfStyleType string style_type =
  case style_type of
    Image _ ->
      Image (Just string)

    _ ->
      style_type


{-| Set the mouseover text of an image

examples:

  > setMouseoverText "A cute catte" (Style { Just (Image Nothing), "https://catte.com/cat.png" })
  Style { Just (Image (Just "A cute catte")), "https://catte.com/cat.png" })

  > setMouseoverText "some text" (Style { Just Code, "Enum.map/2" })
  Style { Just Code, "Enum.map/2" }
-}
setMouseoverText : String -> Style -> Style
setMouseoverText string (Style { link_wrapper, text }) =
  case link_wrapper of
    Just style_type ->
      Just (setMouseoverTextOfStyleType string style_type)

    Link href style_type ->
      Link href (setMouseoverTextOfStyleType string style_type)


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
toggleAttribute (StyleAttributes -> StyleAttributes) -> StyleType -> StyleType
toggleAttribute fn style_type =
  case style_type of
    Text style_attributes ->
      Text (fn style_attributes)

    _ ->
      style_type


toggleAttributeFromStyle : (StyleAttributes -> StyleAttributes) -> Style -> Style
toggleAttributeFromStyle fn (Style { link_wrapper, text }) =
  case link_wrapper of
    Just style_type ->
      Style { Just (toggleAttribute fn style_type), text }

    Link href style_type ->
      Style { Link href (toggleAttribute fn style_type), text }


{-| Headings cannot be applied to Code or Image.

examples:

  > toggleHeading (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {False, True, True, True, True}), "Hello World" }

  > toggleHeading (Style { Just (Text {False, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleHeading (Style { Just Code, "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleHeading (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Image Nothing), "https://google.com/google.png" }

-}
toggleHeading : Style -> Style
toggleHeading =
  toggleAttributeFromStyle toggleHeadingAttribute


{-| Bold cannot be applied to Code or Image.

examples:

  > toggleBold (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, False, True, True, True}), "Hello World" }

  > toggleBold (Style { Just (Text {True, False, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleBold (Style { Just Code, "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleBold (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Image Nothing), "https://google.com/google.png" }

-}
toggleBold : Style -> Style
toggleBold =
  toggleAttributeFromStyle toggleBoldAttribute


{-| Italic cannot be applied to Code or Image.

examples:

  > toggleItalic (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, False, True, True}), "Hello World" }

  > toggleItalic (Style { Just (Text {True, True, False, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleItalic (Style { Just Code, "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleItalic (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Image Nothing), "https://google.com/google.png" }

-}
toggleItalic : Style -> Style
toggleItalic =
  toggleAttributeFromStyle toggleItalicAttribute


{-| Underline cannot be applied to Code or Image.

examples:

  > toggleUnderline (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, False, True}), "Hello World" }

  > toggleUnderline (Style { Just (Text {True, True, True, False, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleUnderline (Style { Just Code, "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleUnderline (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Image Nothing), "https://google.com/google.png" }

-}
toggleUnderline : Style -> Style
toggleUnderline =
  toggleAttributeFromStyle toggleUnderlineAttribute


{-| Strike cannot be applied to Code or Image.

examples:

  > toggleStrike (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, False}), "Hello World" }

  > toggleStrike (Style { Just (Text {True, True, True, True, False}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleStrike (Style { Just Code, "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleStrike (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Image Nothing), "https://google.com/google.png" }

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
toggleStyleTypeFromStyle fn Style { link_wrapper, text } =
  case link_wrapper of
    Just style_type ->
      Style { Just (fn style_type), text }

    Link href style_type ->
      Style { Link href (fn style_type), text }


{-| toggleImage removes styling (except link) and converts the remaining Text to an Image

examples:

  > toggleImage (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Image Nothing), "Hello World" }

  > toggleImage (Style { Just Code, "Hello World" })
  Style { Just (Image Nothing), "Hello World" }

  > toggleImage (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Text {False, False, False, False, False}), "https://google.com/google.png" }

-}
toggleImage : Style -> Style
toggleImage =
  toggleStyleTypeFromStyle toggleImageFromStyleType


{-| toggleCode removes styling (except link) and converts the remaining Text to Code

examples:

  > toggleCode (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just Code, "Hello World" }

  > toggleCode (Style { Just Code, "Hello World" })
  Style { Just (Text {False, False, False, False, False}), "Hello World" }

  > toggleCode (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just Code, "https://google.com/google.png" }

-}
toggleCode : Style -> Style
toggleCode =
  toggleStyleTypeFromStyle toggleCodeFromStyleType


{-| toggleText ensures the style is of Text type

examples:

  > toggleText (Style { Just (Text {True, True, True, True, True}), "Hello World" })
  Style { Just (Text {True, True, True, True, True}), "Hello World" }

  > toggleText (Style { Just Code, "Hello World" })
  Style { Just (Text {False, False, False, False, False}), "Hello World" }

  > toggleText (Style { Just (Image Nothing), "https://google.com/google.png" })
  Style { Just (Text {False, False, False, False, False}), "https://google.com/google.png" }

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
    Just style_type ->
      "Style { Just (" ++ (serializeStyleTypeToString style_type) ++ "), " ++ text ++ " }"

    Link href style_type ->
      "Style { Link " ++ href ++ " (" ++ (serializeStyleTypeToString style_type) ++ "), " ++ text ++ " }"


renderStyleAttributes : StyleAttributes -> String -> Html msg
renderStyleAttributes attributes str =
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

renderStyleType : StyleType -> String -> Html msg
renderStyleType style_type string =
  case style_type of
    Text attributes ->
      renderStyleAttributes attributes string

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

  > renderStyle (Style { Just (Text {False, True, False, False, False}), "hello world" })
  b [] [ text "hello world" ]

  > renderStyle (Style { Link "https://google.com" (Text {True, True, True, True, True}), "hello world" })
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
renderStyle : Style -> Html msg
renderStyle (Style { link_wrapper, text }) =
  case link_wrapper of
    Just style_type ->
      renderStyleType style_type text

    Link url style_type ->
      a [ href url ] [ renderStyleType style_type text ]
