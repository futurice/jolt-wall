module Components.JoltMessage exposing (..)

import Html exposing (Html, text, div, img, span)
import Html.Attributes exposing (src, class)
import Maybe exposing (withDefault)
import String exposing (padLeft, left, dropLeft, split, startsWith)
import Date exposing (minute, hour, day, month)
import Types.Message exposing (Message)
import Types.User exposing (User)
import Helpers.Jolts exposing (userTag, joltTag)


joltMessage : Message -> List User -> List (Html msg)
joltMessage jolt users =
    let
        joltUser : User
        joltUser =
            users
                |> List.filter (\user -> user.id == jolt.user)
                |> List.head
                |> withDefault (User "" "unknown" "")

        joltedUsers : List User
        joltedUsers =
            jolt.tags
                |> List.filter (\tag -> startsWith userTag tag)
                |> List.map (dropLeft <| String.length userTag)
                |> List.filterMap
                    (\userIdString ->
                        users
                            |> List.filter (\user -> user.id == userIdString)
                            |> List.head
                    )

        joltSent : String
        joltSent =
            (toString <| day jolt.sent)
                ++ " "
                ++ (toString <| month jolt.sent)
                ++ " at "
                ++ (toString <| hour jolt.sent)
                ++ ":"
                ++ (padLeft 2 '0' <| toString <| minute jolt.sent)

        addEllipsis : String -> String
        addEllipsis string =
            if String.length string == 140 then
                string ++ "..."
            else
                string

        joltSpan : Html msg
        joltSpan =
            span [ class "jolt-in-message" ] [ text "jolt" ]

        stringToSpan : String -> Html msg
        stringToSpan string =
            span [] [ text string ]

        extractHeadTailAsEmptyArrays : List (Html msg) -> ( List (Html msg), List (Html msg) )
        extractHeadTailAsEmptyArrays spans =
            ( withDefault []
                (List.head spans
                    |> Maybe.map List.singleton
                )
            , withDefault [] (List.tail spans)
            )

        foldJolts : Html msg -> List (Html msg) -> List (Html msg)
        foldJolts span cum =
            List.concat [ cum, [ joltSpan ], [ span ] ]

        addJoltsBetweenItems : ( List (Html msg), List (Html msg) ) -> List (Html msg)
        addJoltsBetweenItems ( spansHead, spansTail ) =
            List.foldl (foldJolts) spansHead spansTail

        joltMessage : List (Html msg)
        joltMessage =
            withDefault "Empty content!? Thanks Flowdock!" jolt.content
                |> left 140
                |> addEllipsis
                |> split joltTag
                |> List.map stringToSpan
                |> extractHeadTailAsEmptyArrays
                |> addJoltsBetweenItems

        joltArrow =
            if List.isEmpty joltedUsers then
                ""
            else
                ">"

        renderJoltedUsers =
            joltedUsers
                |> List.map (\user -> (img [ class "jolt-participants__user-img--right", src user.avatar ] []))
    in
        [ div [ class "jolt-participants" ]
            [ div [] [ img [ class "jolt-participants__user-img", src joltUser.avatar ] [] ]
            , div [ class "jolt-participants__arrow" ] [ text joltArrow ]
            , div [] renderJoltedUsers
            ]
        , div [ class "jolt-content" ] joltMessage
        , div [ class "jolt-details" ]
            [ text <| joltSent ++ " by " ++ joltUser.nick
            ]
        ]
