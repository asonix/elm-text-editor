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
    (Content, setLinkHref, updateText, appendText, toggleCode, toggleImage,
    toggleStrike, toggleUnderline, toggleItalic, toggleBold, toggleHeading,
    toggleLink, serializeToString, renderStyle)

{-|
@docs Content

@docs setLinkHref, updateText, appendText, toggleCode, toggleImage, toggleStrike
@docs toggleUnderline, toggleItalic, toggleBold, toggleHeading, toggleLink
@docs serializeToString, renderStyle
-}

import Html exposing (..)


{-| Internal representation of Styled Text.
-}
type Content
  = Text String
  | Image String
  | Code String
  | Heading Content
  | Link String Content
  | Bold Content
  | Italic Content
  | Underline Content
  | Strike Content


{-| Set the URL of a link in a content chain

examples:

  >appendText ", hello" (Bold (Italic (Text "hi")))
  Bold (Italic (Text "hi, hello"))

  >appendText ", hello" (Link "https://yahoo.com" (Bold (Text "hi")))
  Link "https://yahoo.com" (Bold (Text "hi, hello"))

-}
appendText : String -> Content -> Content
appendText text content =
  case content of
    Text str ->
      Text (str ++ text)

    Image str ->
      Image (str ++ text)

    Code str ->
      Code (str ++ text)

    Heading content ->
      Heading (appendText text content)

    Link href content ->
      Link href (appendText text content)

    Bold content ->
      Bold (appendText text content)

    Italic content ->
      Italic (appendText text content)

    Underline content ->
      Underline (appendText text content)

    Strike content ->
      Strike (appendText text content)


{-| Set the URL of a link in a content chain

examples:

  >updateText "hello" (Bold (Italic (Text "hi")))
  Bold (Italic (Text "hello"))

  >updateText "hello" (Link "https://yahoo.com" (Bold (Text "hi")))
  Link "https://yahoo.com" (Bold (Text "hello"))

-}
updateText : String -> Content -> Content
updateText text content =
  case content of
    Text _ ->
      Text text

    Image _ ->
      Image text

    Code _ ->
      Code text

    Heading content ->
      Heading (updateText text content)

    Link href content ->
      Link href (updateText text content)

    Bold content ->
      Bold (updateText text content)

    Italic content ->
      Italic (updateText text content)

    Underline content ->
      Underline (updateText text content)

    Strike content ->
      Strike (updateText text content)


{-| Set the URL of a link in a content chain

examples:

  >setLinkHref "https://google.com" (Bold (Italic (Text "hi")))
  Bold (Italic (Link "https://google.com" (Text "hi")))

  >setLinkHref "https://google.com" (Link "https://yahoo.com" (Bold (Text "hi")))
  Link "https://google.com" (Bold (Text "hi"))

-}
setLinkHref : String -> Content -> Content
setLinkHref href content =
  case content of
    Text str ->
      Link href (Text str)

    Image str ->
      Link href (Image str)

    Code str ->
      Link href (Code str)

    Heading content ->
      Heading (setLinkHref href content)

    Link _ content ->
      Link href content

    Bold content ->
      Bold (setLinkHref href content)

    Italic content ->
      Italic (setLinkHref href content)

    Underline content ->
      Underline (setLinkHref href content)

    Strike content ->
      Strike (setLinkHref href content)

{- Form a hierarchy of styles. Order:

Link (Heading (Bold (Italic (Underline (Strike (Text str))))))

Link (Image str)
Link (Code str)
-}


{-| Links can be applied to any type of content.

They are also applied in front of any other styles

examples:

  > toggleLink "https://google.com" (Heading (Text "hello world"))
  Link "https://google.com" (Heading (Text "hello world"))

  > toggleLink "" (Link "https://google.com" (Text "hello world"))
  Text "hello world"

-}
toggleLink : String -> Content -> Content
toggleLink href current_content =
  case current_content of
    Text str ->
      Link href (Text str)

    Image str ->
      Link href (Image str) -- Images can be links

    Code str ->
      Link href (Code str) -- Code can be a link

    Link _ content ->
      content -- remove link if present

    Heading content ->
      Link href Heading content -- don't style headings

    Bold content ->
      Link href (Bold content)

    Italic content ->
      Link href (Italic content)

    Underline content ->
      Link href (Underline content)

    Strike content ->
      Link href (Strike content)


{-| Headings cannot be applied to Code or Image.

They are also applied in front of all styles except link

examples:

  > toggleHeading (Bold (Text "hello world"))
  Heading (Bold (Text "hello world"))

  > toggleHeading (Heading (Text "hello world"))
  Text "hello world"

  > toggleHeading (Link "https://google.com (Heading (Text "hello world")))
  Link "https://google.com" (Text "hello world")

-}
toggleHeading : Content -> Content
toggleHeading current_content =
  case current_content of
    Text str ->
      Heading (Text str)

    Image str ->
      Image str -- don't style images

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleHeading content)

    Heading content ->
      content -- remove heading if applied

    Bold content ->
      Heading (Bold content)

    Italic content ->
      Heading (Italic content)

    Underline content ->
      Heading (Underline content)

    Strike content ->
      Heading (Strike content)


{-| Bold cannot be applied to Code or Image.

Bold is preceded by Link and Heading

examples:

  > toggleBold (Italic (Text "hello world"))
  Bold (Italic (Text "hello world"))

  > toggleBold (Bold (Italic (Text "hello world")))
  Italic (Text "hello world")

  > toggleBold (Link "https://google.com" (Italic (Text "hello world")))
  Link "https://google.com" (Bold (Italic (Text "hello world")))

-}
toggleBold : Content -> Content
toggleBold current_content =
  case current_content of
    Text str ->
      Bold (Text str)

    Image str ->
      Image str -- don't style images

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleBold content)

    Heading content ->
      Heading (toggleBold content)

    Bold content ->
      content -- remove style if applied

    Italic content ->
      Bold (Italic content)

    Underline content ->
      Bold (Underline content)

    Strike content ->
      Bold (Strike content)


{-| Italic cannot be applied to Code or Image.

Italic is preceded by Link, Heading, and Bold

examples:

  > toggleItalic (Bold (Text "hello world"))
  Bold (Italic (Text "hello world"))

  > toggleItalic (Bold (Italic (Text "hello world")))
  Bold (Text "hello world")

-}
toggleItalic : Content -> Content
toggleItalic current_content =
  case current_content of
    Text str ->
      Italic (Text str)

    Image str ->
      Image str -- don't style images

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleItalic content)

    Heading content ->
      Heading (toggleItalic content)

    Bold content ->
      Bold (toggleItalic content)

    Italic content ->
      content -- remove style if applied

    Underline content ->
      Italic (Underline content)

    Strike content ->
      Italic (Strike content)


{-| Underline cannot be applied to Code or Image.

Underline is preceded by Link, Heading, Bold, and Italic

examples:

  > toggleUnderline (Bold (Text "hello world"))
  Bold (Underline (Text "hello world"))

  > toggleUnderline (Bold (Underline (Text "hello world")))
  Bold (Text "hello world")

-}
toggleUnderline : Content -> Content
toggleUnderline current_content =
  case current_content of
    Text str ->
      Underline (Text str)

    Image str ->
      Image str -- don't style images

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleUnderline content)

    Heading content ->
      Heading (toggleUnderline content)

    Bold content ->
      Bold (toggleUnderline content)

    Italic content ->
      Italic (toggleUnderline content)

    Underline content ->
      content -- remove style if applied

    Strike content ->
      Underline (Strike content)


{-| Strike cannot be applied to Code or Image.

Strike is preceded by Link, Heading, Bold, Italic, and Underline

examples:

  > toggleStrike (Bold (Text "hello world"))
  Bold (Strike (Text "hello world"))

  > toggleStrike (Bold (Strike (Text "hello world")))
  Bold (Text "hello world")

  > toggleStrike (Link "https://google.com" (Bold (Text "hello world")))
  Link "https://google.com" (Bold (Strike (Text "hello world")))

-}
toggleStrike : Content -> Content
toggleStrike current_content =
  case current_content of
    Text str ->
      Strike (Text str)

    Image str ->
      Image str -- don't style images

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleStrike content)

    Heading content ->
      Heading (toggleStrike content)

    Bold content ->
      Bold (toggleStrike content)

    Italic content ->
      Italic (toggleStrike content)

    Underline content ->
      Underline (toggleStrike content)

    Strike content ->
      content -- remove style if applied


{-| toggleImage removes styling (except link) and converts the remaining Text to an Image

examples:

  > toggleImage (Bold (Text "https://google.com/google.png"))
  Image "hello world"

  > toggleImage (Image "https://google.com/google.png")
  Text "https://google.com/google.png"

  > toggleImage (Link "https://google.com" (Text "https://google.com/google.png"))
  Link "https://google.com" (Image "https://google.com/google.png"

-}
toggleImage : Content -> Content
toggleImage current_content =
  case current_content of
    Text str ->
      Image str

    Image str ->
      Text str

    Code str ->
      Code str -- don't style code

    Link href content ->
      Link href (toggleImage content)

    Heading content ->
      toggleImage content

    Bold content ->
      toggleImage content

    Italic content ->
      toggleImage content

    Underline content ->
      toggleImage content

    Strike content ->
      toggleImage content


{-| toggleCode removes styling (except link) and converts the remaining Text to Code

examples:

  > toggleCode (Bold (Text "hello world"))
  Code "hello world"

  > toggleCode (Code "hello world")
  Text "hello world"

  > toggleCode (Link "https://google.com (Text "hello world"))
  Link "https://google.com" (Code "hello world")

-}
toggleCode : Content -> Content
toggleCode current_content =
  case current_content of
    Text str ->
      Code str

    Image str ->
      Image str -- don't style images

    Code str ->
      Text str

    Link href content ->
      Link href (toggleCode content)

    Heading content ->
      toggleCode content

    Bold content ->
      toggleCode content

    Italic content ->
      toggleCode content

    Underline content ->
      toggleCode content

    Strike content ->
      toggleCode content


{-| Provide a string representation of styling

This is for debugging use only
-}
serializeToString : Content -> String
serializeToString content =
  case content of
    Text str ->
      "(Text \"" + str + "\")"

    Image str ->
      "(Image \"" + str + "\")"

    Code str ->
      "(Code \"" + str + "\")"

    Link href content ->
      "(Link \"" + href + \" " + (serializeToString content) + ")"

    Heading content ->
      "(Heading " + (serializeToString content) + ")"

    Bold content ->
      "(Bold " + (serializeToString content) + ")"

    Italic content ->
      "(Italic " + (serializeToString content) + ")"

    Underline content ->
      "(Underline " + (serializeToString content) + ")"

    Strike content ->
      "(Strike " + (serializeToString content) + ")"


{-| Converts a Content into HTML elements

Dom Nodes galore.

This could be refactored to apply classes to an external span with the addition
of a helper function

examples:

  > renderStyle (Bold (Text "hello world"))
  b [] [ text "hello world" ]

  > renderStyle (Link "https://google.com" (Heading (Bold (Italic (Underline (Strike (Text "hello world")))))))
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
renderStyle : Content -> Html
renderStyle current_content =
  case current_content of
    Text str ->
      text str

    Image str ->
      image [ src str ] []

    Code str ->
      code [] [ text str ]

    Link link content ->
      a [ href link ] [ renderStyle content ]

    Heading content ->
      h3 [] [ renderStyle content ]

    Bold content ->
      b [] [ renderStyle content ]

    Italic content ->
      i [] [ renderStyle content ]

    Underline content ->
      u [] [ renderStyle content ]

    Strike content ->
      span [ class "strike" ] [ renderStyle content ]
