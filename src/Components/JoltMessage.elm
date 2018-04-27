module Components.JoltMessage exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)
import Maybe exposing (withDefault)
import String exposing (padLeft, left)
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
    in
        [ div [ class "jolt-content" ]
            [ (withDefault "Empty content!? Thanks Flowdock!" jolt.content)
                |> left 140
                |> (\string ->
                        if String.length string == 140 then
                            string ++ "..."
                        else
                            string
                   )
                |> text
            ]
        , div [ class "jolt-details" ]
            [ div [ class "jolt-user" ]
                [ img [ src joltUser.avatar ] []
                , text <| " " ++ joltUser.nick
                ]
            , div [ class "jolt-sent" ] [ text joltSent ]
            ]
        ]
