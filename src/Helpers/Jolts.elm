module Helpers.Jolts exposing (..)

import List exposing (reverse, filter, length)
import Date exposing (year, month)
import Time exposing (Time)
import Helpers.Dates exposing (getMonthFromTime, getYearFromTime)
import Types.Message exposing (Message)


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
