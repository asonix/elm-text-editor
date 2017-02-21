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


module Navigation
    exposing
        ( Navigation
        , init
        , moveLeft
        , moveRight
        , delete
        , newParagraph
        , toggleStyle
        , view
        , takeInput
        , viewDebug
        )

{-|
@docs Navigation
@docs init
@docs moveLeft
@docs moveRight
@docs delete
@docs newParagraph
@docs toggleStyle
@docs view
@docs takeInput
@docs viewDebug
-}

import Content exposing (..)
import Paragraph exposing (..)
import Styles exposing (..)
import Html exposing (..)


{-| Wrapper around Content type while maintaining previous and next strings
-}
type alias Navigation =
    { content : Content
    , previous_text : String
    , next_text : String
    }


shiftLeft : String -> String -> ( String, String )
shiftLeft previous next =
    ( String.dropRight 1 previous, (String.right 1 previous) ++ next )


shiftRight : String -> String -> ( String, String )
shiftRight previous next =
    ( previous ++ (String.left 1 next), String.dropLeft 1 next )


{-| create a deault Navigation
-}
init : Navigation
init =
    { content = Content.init
    , previous_text = ""
    , next_text = ""
    }


{-| Cycle left through Navigation
-}
moveLeft : Navigation -> Navigation
moveLeft navigation =
    navigation
        |> moveLeftThroughText
        |> moveLeftThroughParagraph
        |> moveLeftThroughContent


{-| Cycle right through Navigation
-}
moveRight navigation =
    navigation
        |> moveRightThroughText
        |> moveRightThroughParagraph
        |> moveRightThroughContent


{-| Cycle through current style's text
-}
moveLeftThroughText : Navigation -> ( Bool, Navigation )
moveLeftThroughText navigation =
    let
        ( new_previous, new_next ) =
            shiftLeft navigation.previous_text navigation.next_text
    in
        if not (String.isEmpty navigation.previous_text) then
            ( True
            , { navigation
                | previous_text = new_previous
                , next_text = new_next
              }
            )
        else
            ( False, navigation )


{-| Cycle through current style's text
-}
moveRightThroughText : Navigation -> ( Bool, Navigation )
moveRightThroughText navigation =
    let
        ( new_previous, new_next ) =
            shiftRight navigation.previous_text navigation.next_text
    in
        if not (String.isEmpty navigation.next_text) then
            ( True
            , { navigation
                | previous_text = new_previous
                , next_text = new_next
              }
            )
        else
            ( False, navigation )


{-| Cycle through current Paragraph's styles
-}
moveLeftThroughParagraph : ( Bool, Navigation ) -> ( Bool, Navigation )
moveLeftThroughParagraph ( complete, navigation ) =
    let
        content : Content
        content =
            Content.updateCurrentParagraph
                (Paragraph.selectPreviousStyle)
                navigation.content

        ( new_previous_text, new_next_text ) =
            content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText
                |> flip (shiftLeft) ""
    in
        if
            not complete
                && Paragraph.hasPreviousStyle
                    (Content.currentParagraph navigation.content)
        then
            ( True
            , { navigation
                | content = content
                , previous_text = new_previous_text
                , next_text = new_next_text
              }
            )
        else
            ( complete, navigation )


{-| Cycle through current Paragraph's styles
-}
moveRightThroughParagraph : ( Bool, Navigation ) -> ( Bool, Navigation )
moveRightThroughParagraph ( complete, navigation ) =
    let
        content : Content
        content =
            Content.updateCurrentParagraph
                (Paragraph.selectNextStyle)
                navigation.content

        ( new_previous_text, new_next_text ) =
            content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText
                |> shiftRight ""
    in
        if
            not complete
                && Paragraph.hasNextStyle
                    (Content.currentParagraph navigation.content)
        then
            ( True
            , { navigation
                | content = content
                , previous_text = new_previous_text
                , next_text = new_next_text
              }
            )
        else
            ( complete, navigation )


{-| Cycle through Content's Paragraphs
-}
moveLeftThroughContent : ( Bool, Navigation ) -> Navigation
moveLeftThroughContent ( complete, navigation ) =
    let
        content : Content
        content =
            navigation.content
                |> Content.selectPreviousParagraph
                |> Content.updateCurrentParagraph (Paragraph.toEnd)

        new_previous_text : String
        new_previous_text =
            content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText

        new_next_text : String
        new_next_text =
            ""
    in
        if
            not complete
                && Content.hasPreviousParagraph navigation.content
        then
            { navigation
                | content = content
                , previous_text = new_previous_text
                , next_text = new_next_text
            }
        else
            navigation


{-| Cycle through Content's Paragraphs
-}
moveRightThroughContent : ( Bool, Navigation ) -> Navigation
moveRightThroughContent ( complete, navigation ) =
    let
        content : Content
        content =
            navigation.content
                |> Content.selectNextParagraph
                |> Content.updateCurrentParagraph (Paragraph.toStart)

        new_previous_text : String
        new_previous_text =
            ""

        new_next_text : String
        new_next_text =
            content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText
    in
        if not complete && Content.hasNextParagraph navigation.content then
            { navigation
                | content = content
                , previous_text = new_previous_text
                , next_text = new_next_text
            }
        else
            navigation


{-| Deletes text from current style or text from previous style or paragraph
-}
delete : Navigation -> Navigation
delete navigation =
    navigation
        |> deleteText
        |> deleteAcrossStyles
        |> deleteAcrossParagraphs


deleteAcrossParagraphs : ( Bool, Navigation ) -> Navigation
deleteAcrossParagraphs ( complete, navigation ) =
    let
        new_content : Content
        new_content =
            mergeWithPreviousParagraph navigation.content

        new_previous_text : String
        new_previous_text =
            new_content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText
    in
        if not complete && Content.hasPreviousParagraph navigation.content then
            { navigation
                | content = new_content
                , previous_text = new_previous_text
                , next_text = ""
            }
        else
            navigation


deleteAcrossStyles : ( Bool, Navigation ) -> ( Bool, Navigation )
deleteAcrossStyles ( complete, navigation ) =
    let
        new_content : Content
        new_content =
            if String.isEmpty navigation.next_text then
                navigation.content
                    |> Content.updateCurrentParagraph
                        (Paragraph.removeCurrentStyle)
                    |> Content.updateCurrentParagraph
                        (Paragraph.updateCurrentStyle
                            (Styles.updateText
                                (String.dropRight 1)
                            )
                        )
            else
                navigation.content
                    |> Content.updateCurrentParagraph
                        (Paragraph.selectPreviousStyle)
                    |> Content.updateCurrentParagraph
                        (Paragraph.updateCurrentStyle
                            (Styles.updateText
                                (String.dropRight 1)
                            )
                        )

        new_previous_text : String
        new_previous_text =
            new_content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.getText
    in
        if
            not complete
                && Paragraph.hasPreviousStyle
                    (Content.currentParagraph navigation.content)
        then
            ( True
            , { navigation
                | content = new_content
                , previous_text = new_previous_text
                , next_text = ""
              }
            )
        else
            ( complete, navigation )


deleteText : Navigation -> ( Bool, Navigation )
deleteText navigation =
    let
        new_previous_text : String
        new_previous_text =
            String.dropRight 1 navigation.previous_text

        combined_text : String
        combined_text =
            new_previous_text ++ navigation.next_text

        new_content : Content
        new_content =
            navigation.content
                |> Content.updateCurrentParagraph
                    (Paragraph.updateCurrentStyle
                        (Styles.setText combined_text)
                    )
    in
        if not (String.isEmpty navigation.previous_text) then
            ( True
            , { navigation
                | content = new_content
                , previous_text = new_previous_text
                , next_text = navigation.next_text
              }
            )
        else
            ( False, navigation )


{-| Create a new paragraph
-}
newParagraph : Navigation -> Navigation
newParagraph navigation =
    let
        last_style : Style
        last_style =
            navigation.content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.setText navigation.previous_text

        first_style : Style
        first_style =
            Styles.setText navigation.next_text last_style

        next_styles : List Style
        next_styles =
            navigation.content
                |> Content.currentParagraph
                |> Paragraph.nextStyles

        new_content : Content
        new_content =
            navigation.content
                |> Content.updateCurrentParagraph (Paragraph.clearNextStyles)
                |> Content.updateCurrentParagraph
                    (Paragraph.setCurrentStyle last_style)
                |> Content.insertParagraph (first_style :: next_styles)
    in
        { navigation
            | content = new_content
            , previous_text = ""
            , next_text = navigation.next_text
        }


{-| Make new current Style, splitting the current Paragraph's current Style
-}
toggleStyle : (Style -> Style) -> Navigation -> Navigation
toggleStyle toggle navigation =
    let
        current_style : Style
        current_style =
            navigation.content
                |> Content.currentParagraph
                |> Paragraph.currentStyleWithDefault Styles.empty
                |> Styles.setText ""

        left_style : Maybe Style
        left_style =
            if String.isEmpty navigation.previous_text then
                Nothing
            else
                Just (Styles.setText navigation.previous_text current_style)

        right_style : Maybe Style
        right_style =
            if String.isEmpty navigation.next_text then
                Nothing
            else
                Just (Styles.setText navigation.next_text current_style)

        new_current_style : Style
        new_current_style =
            current_style
                |> toggle

        new_content : Content
        new_content =
            navigation.content
                |> Content.updateCurrentParagraph (Paragraph.insertBefore left_style)
                |> Content.updateCurrentParagraph (Paragraph.selectNextStyle)
                |> Content.updateCurrentParagraph (Paragraph.insertAfter right_style)
                |> Content.updateCurrentParagraph (Paragraph.selectPreviousStyle)
                |> Content.updateCurrentParagraph (Paragraph.setCurrentStyle new_current_style)
    in
        { navigation
            | content = new_content
            , previous_text = ""
            , next_text = ""
        }


{-| Render the current Navigation to Html msg
-}
view : Navigation -> Html msg
view navigation =
    Content.view navigation.content


{-| Add Input to the navigation structure
-}
takeInput : String -> Navigation -> Navigation
takeInput input navigation =
    let
        new_previous_text : String
        new_previous_text =
            navigation.previous_text ++ input

        combined_text : String
        combined_text =
            new_previous_text ++ navigation.next_text

        content : Content
        content =
            Content.updateCurrentParagraph
                (Paragraph.updateCurrentStyle
                    (Styles.setText combined_text)
                )
                navigation.content
    in
        { navigation
            | content = content
            , previous_text = new_previous_text
            , next_text = navigation.next_text
        }


{-| view Debug info as HTML
-}
viewDebug : Navigation -> Html msg
viewDebug navigation =
    Html.blockquote []
        [ Html.h3 [] [ text "Navigation" ]
        , Html.blockquote []
            [ Html.h3 [] [ text "Strings" ]
            , Html.p []
                [ Html.text "previous: "
                , Html.text navigation.previous_text
                ]
            , Html.p []
                [ Html.text "next: "
                , Html.text navigation.next_text
                ]
            ]
        , Content.viewDebug navigation.content
        ]
