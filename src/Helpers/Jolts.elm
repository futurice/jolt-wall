module Helpers.Jolts exposing (..)

import List exposing (reverse, filter, length, map)
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


validJolts : List Message -> List Message
validJolts flowMessages =
    flowMessages
        |> reverse
        |> filter (\item -> item.event == "message")
        |> filter (\item -> item.content /= Nothing)


joltsCountInMonth : List Message -> Time -> Int
joltsCountInMonth flowMessages time =
    flowMessages
        |> validJolts
        |> filter
            (\jolt ->
                (month jolt.sent)
                    == (getMonthFromTime time)
                    && (year jolt.sent)
                    == (getYearFromTime time)
            )
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
