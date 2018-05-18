module Components.JoltCounts exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class, src)
import Date exposing (Month)
import Types.Model exposing (Model)
import Helpers.Jolts exposing (joltsCountInMonth, joltsInPreviousMonths)


joltCounts : Model -> Html msg
joltCounts model =
    let
        joltsThisMonth =
            joltsCountInMonth model.flowMessages model.currentTime

        joltsCountInPreviousMonths =
            joltsInPreviousMonths model.flowMessages model.currentTime 3

        renderJoltHistory : List ( Month, Int ) -> List (Html msg)
        renderJoltHistory monthJoltsList =
            monthJoltsList
                |> List.map
                    (\monthJolts ->
                        let
                            monthString =
                                monthJolts
                                    |> Tuple.first
                                    |> toString

                            joltsString =
                                monthJolts
                                    |> Tuple.second
                                    |> toString
                        in
                            div [ class "jolts-count__history" ]
                                [ div [] [ text monthString ]
                                , div [] [ text joltsString ]
                                ]
                    )
    in
        div [ class "jolts-counts" ]
            [ div [ class "jolts-count" ]
                [ div [ class "jolts-count__hero-number" ] [ text <| toString joltsThisMonth ]
                , div [ class "jolts-count__hero-text" ] [ text "Jolts this month" ]
                ]
            , div [ class "jolts-count" ] (renderJoltHistory joltsCountInPreviousMonths)
            , div [ class "munich-logo" ] [ img [ src "/logo.svg", class "munich-logo__image" ] [] ]
            ]
