module Components.JoltMessages exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (id, class)
import List exposing (indexedMap, isEmpty, take)
import Components.JoltMessage exposing (joltMessage)
import Types.Msg exposing (..)
import Types.Model exposing (Model)
import Types.Message exposing (Message)
import Types.User exposing (User)
import Helpers.Jolts exposing (validJolts)


joltFeedLength : Int
joltFeedLength =
    12


joltMessages : Model -> Html Msg
joltMessages model =
    div [ class "jolt-messages" ] (renderJolts model.flowMessages model.flowUsers)


renderJolts : List Message -> List User -> List (Html Msg)
renderJolts messages users =
    if isEmpty messages then
        [ div [ class "no-jolts" ] [ text "KEINE JOLTS!!! Was ist los!" ] ]
    else
        validJolts messages
            |> take joltFeedLength
            |> indexedMap
                (\index item ->
                    let
                        htmlId =
                            ("jolt-message-" ++ toString index)
                    in
                        div [ id htmlId, class "jolt-message" ] (joltMessage item users)
                )
