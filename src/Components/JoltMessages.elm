module Components.JoltMessages exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (id, class)
import List exposing (indexedMap, isEmpty, take)
import Components.JoltMessage exposing (joltMessage)
import Types.Msg exposing (..)
import Types.Model exposing (Model)
import Helpers.Jolts exposing (validJolts)


joltMessages : Model -> Html Msg
joltMessages model =
    let
        messages =
            model.flowMessages

        renderJolts =
            if isEmpty messages then
                [ div [ class "no-jolts" ] [ text "KEINE JOLTS!!! Was ist los!" ] ]
            else
                validJolts messages
                    |> take 12
                    |> indexedMap
                        (\index item ->
                            let
                                htmlId =
                                    ("jolt-message-" ++ toString index)
                            in
                                div [ id htmlId, class "jolt-message" ] (joltMessage item model.flowUsers)
                        )
    in
        div [ class "jolt-messages" ] renderJolts
