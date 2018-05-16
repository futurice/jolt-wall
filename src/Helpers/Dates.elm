module Helpers.Dates exposing (..)

import List exposing (range, map)
import Time exposing (Time)
import Date exposing (Date, fromTime, month, year)
import Time.DateTime as DateTime exposing (DateTime, fromTimestamp, toTimestamp, toISO8601, addMonths)


getYearFromTime : Time -> Int
getYearFromTime time =
    time
        |> fromTime
        |> year


getMonthFromTime : Time -> Date.Month
getMonthFromTime time =
    time
        |> fromTime
        |> month


getPreviousMonths : Time -> Int -> List Time
getPreviousMonths time monthsToGet =
    let
        addMonthsWithDate =
            time
                |> fromTimestamp
                |> flip addMonths
    in
        monthsToGet
            |> range 1
            |> map
                (negate
                    >> addMonthsWithDate
                    >> toTimestamp
                )
