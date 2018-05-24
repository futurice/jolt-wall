module Helpers.Jolts exposing (..)

import List exposing (reverse, filter, length, map, any)
import String exposing (contains)
import Date exposing (Month, year, month)
import Time exposing (Time)
import Helpers.Dates exposing (getMonthFromTime, getYearFromTime, getPreviousMonths)
import Types.Message exposing (Message)


joltTag : String
joltTag =
    ":jolt:"


userTag : String
userTag =
    ":user:"


isMessage : Message -> Bool
isMessage { event } =
    event == "message"


hasContent : Message -> Bool
hasContent { content } =
    content /= Nothing


isUserTagged : Message -> Bool
isUserTagged { tags } =
    any (contains userTag) tags


validJolts : List Message -> List Message
validJolts flowMessages =
    flowMessages
        |> filter isMessage
        |> filter hasContent
        |> filter isUserTagged
        |> reverse


joltInMonth : Time -> Message -> Bool
joltInMonth time { sent } =
    month sent
        == getMonthFromTime time
        && year sent
        == getYearFromTime time


joltsCountInMonth : List Message -> Time -> Int
joltsCountInMonth flowMessages time =
    flowMessages
        |> validJolts
        |> filter (joltInMonth time)
        |> length


joltsInPreviousMonths : List Message -> Time -> Int -> List ( Month, Int )
joltsInPreviousMonths flowMessages comparedTime monthsToGet =
    getPreviousMonths comparedTime monthsToGet
        |> map
            (\time ->
                let
                    month =
                        getMonthFromTime time

                    joltAmount =
                        joltsCountInMonth flowMessages time
                in
                    ( month, joltAmount )
            )
