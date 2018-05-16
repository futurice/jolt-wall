module Components.JoltMessage exposing (..)

import Html exposing (Html, text, div, img, span)
import Html.Attributes exposing (src, class)
import Maybe exposing (withDefault)
import String exposing (padLeft, left, dropLeft, split)
import Date exposing (minute, hour, day, month)
import Types.Message exposing (Message)
import Types.User exposing (User)


joltMessage : Message -> List User -> List (Html msg)
joltMessage jolt users =
    let
        joltUser : User
        joltUser =
            users
                |> List.filter (\user -> user.id == jolt.user)
                |> List.head
                |> withDefault (User "" "unknown" "")

        joltedUsers =
            withDefault "" jolt.content
                |> String.words
                |> List.filter (\word -> left 1 word == "@")
                |> List.map (dropLeft 1)
                |> List.filterMap
                    (\nickString ->
                        users
                            |> List.filter (\user -> user.nick == nickString)
                            |> List.head
                    )
                |> Debug.log "jolted users"

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

        splitWithJolt : String -> List String
        splitWithJolt string =
            split ":jolt:" string

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
                |> splitWithJolt
                |> List.map stringToSpan
                |> extractHeadTailAsEmptyArrays
                |> addJoltsBetweenItems

        renderJoltedUsers =
            joltedUsers
                |> List.map (\user -> (img [ class "jolt-participants__user-img--right", src user.avatar ] []))
    in
        [ div [ class "jolt-participants" ]
            [ div [] [ img [ class "jolt-participants__user-img", src joltUser.avatar ] [] ]
            , div [ class "jolt-participants__arrow" ] [ text ">" ]
            , div [] renderJoltedUsers
            ]
        , div [ class "jolt-content" ] joltMessage
        , div [ class "jolt-details" ]
            [ text <| joltSent ++ " by " ++ joltUser.nick
            ]
        ]
