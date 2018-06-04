module Components.JoltCounts exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class, src)
import Date exposing (Month)
import Types.Model exposing (Model)
import Helpers.Jolts exposing (joltsCountInMonth, joltsInPreviousMonths)


joltHistoryLength : number
joltHistoryLength =
    3


joltCounts : Model -> Html msg
joltCounts model =
    let
        joltsThisMonth =
            joltsCountInMonth model.flowMessages model.currentTime

        joltsCountInPreviousMonths =
            joltsInPreviousMonths model.flowMessages model.currentTime joltHistoryLength
    in
        div [ class "jolts-counts" ]
            [ div [ class "jolts-count" ]
                [ div [ class "jolts-count__hero-number" ] [ text <| toString joltsThisMonth ]
                , div [ class "jolts-count__hero-text" ] [ text "Jolts this month" ]
                ]
            , div [ class "jolts-count" ] (renderJoltHistory joltsCountInPreviousMonths)
            , div [ class "munich-logo" ] [ img [ src "/logo.svg", class "munich-logo__image" ] [] ]
            ]


renderJoltHistory : List ( Month, Int ) -> List (Html msg)
renderJoltHistory monthJoltsList =
    monthJoltsList
        |> List.map
            (\( month, joltCount ) ->
                div [ class "jolts-count__history" ]
                    [ div [] [ text <| toString month ]
                    , div [] [ text <| toString joltCount ]
                    ]
            )
