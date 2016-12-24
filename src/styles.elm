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
