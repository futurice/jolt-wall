module Components.JoltMessages exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (id, class)
import List exposing (indexedMap, length, take)
import Components.JoltMessage exposing (joltMessage)
import Types.Model exposing (Model)
import Types.Message exposing (Message)


joltMessages : Model -> List Message -> Html msg
joltMessages model validJolts =
    let
        messages =
            if length model.flowMessages == 0 then
                [ div [ class "no-jolts" ] [ text "KEINE JOLTS!!! Was ist los!" ] ]
            else
                validJolts
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
        div [ class "jolt-messages" ] messages
