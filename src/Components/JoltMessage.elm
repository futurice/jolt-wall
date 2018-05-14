module Components.JoltMessage exposing (..)

import Html exposing (Html, text, div, img, span)
import Html.Attributes exposing (src, class)
import Maybe exposing (withDefault)
import String exposing (padLeft, left, split)
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

        joltSent : String
        joltSent =
            (toString <| day jolt.sent)
                ++ " "
                ++ (toString <| month jolt.sent)
                ++ " @ "
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
            ( case (List.head spans) of
                Just span ->
                    [ span ]

                Nothing ->
                    []
            , case (List.tail spans) of
                Just spans ->
                    spans

                Nothing ->
                    []
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
    in
        [ div [ class "jolt-content" ] joltMessage
        , div [ class "jolt-details" ]
            [ div [ class "jolt-user" ]
                [ img [ class "jolt-user__img", src joltUser.avatar ] []
                , text <| " " ++ joltUser.nick
                ]
            , div [ class "jolt-sent" ] [ text joltSent ]
            ]
        ]
