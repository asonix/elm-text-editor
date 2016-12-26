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
    (Content, stringsToStyle, toggleStyle, setLinkHref, updateText, appendText)


{-|
@docs Content

@docs stringsToStyle, toggleStyle, setLinkHref, updateText, appendText
-}


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



{-| Converts a list of style names into actual content styles.

examples:

  > stringsToStyle [] "test"
  Text "test"

  > stringsToStyle ["bold" "underline"] "hey"
  Bold (Underline (Text "hey"))

  > stringsToStyle ["bold" "link" "https://google.com"] "hello"
  Bold (Link "https://google.com" (Text "hello"))

  > stringsToStyle ["bold" "link"] "beep"
  Bold (Text "beep")

-}
stringsToStyle : List String -> String -> Content
stringsToStyle list string =
  case list of
    [] ->
      Text string

    [style] ->
      stringToStyle style string

    (style1::style2::styles) ->
      case style1 of
        "link" ->
          Link style2 (stringsToStyle styles string)
    
        _ ->
          stringToRecursiveStyle style1 (stringsToStyle (style2::styles) string)


stringToStyle : String -> String -> Content
stringToStyle style string =
  case style of
    "text" ->
      Text string

    "image" ->
      Image string

    "code" ->
      Code string

    _ ->
      stringToRecursiveStyle style (Text string)


stringToRecursiveStyle : String -> Content -> Content
stringToRecursiveStyle string content =
  case string of
    "heading" ->
      Heading content

    "bold" ->
      Bold content

    "italic" ->
      Italic content

    "underline" ->
      Underline content

    "strikethrough" ->
      Strike content

    _ ->
      content


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

{- The functions below this line need to be reworked and/or removed
-}

{- Form a hierarchy of styles. Order:

Link (Heading (Bold (Italic (Underline (Strike (Text str))))))

Link (Image str)
Link (Code str)
-}


{-| Links can be applied to any type of content.

They are also applied in front of any other styles
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


{-| Add or remove styles in a list

examples:

  > toggleStyle "bold" []
  ["bold"]

  > toggleStyle "link" []
  ["link" ""]

  > toggleStyle "link" ["bold"]
  ["link" "" "bold"]

  > toggleStyle "link" ["link" "" "bold"]
  ["bold"]

  > toggleStyle "bold" ["bold"]
  []

-}
toggleStyle : String -> List String -> List String
toggleStyle style list =
  if List.member style list then
    case style of
      "link" ->
        removeTwo "link" list

      _ ->
        List.filter (\x -> x /= style) list

  else
    case style of
      "link" ->
        (style::""::list)

      _ ->
        (style::list)


removeTwo : String -> List String -> List String
removeTwo string list =
  case list of
    [] ->
      []

    [one] ->
      if one == string then
        []

      else
        [one]

    (one::two::list) ->
      if one == string then
        list

      else
        removeTwo string (two::list)
