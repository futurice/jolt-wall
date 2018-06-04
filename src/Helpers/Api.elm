module Helpers.Api exposing (..)

import Json.Decode exposing (Decoder)
import Http exposing (header, emptyBody, expectJson, request)
import Types.Msg exposing (..)
import Types.Message exposing (decodeMessages)
import Types.User exposing (decodeUsers)
import Types.Config exposing (Config)


requestFlow : String -> String -> Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
requestFlow url userToken decodeBody messageAfterResponse =
    let
        authorization =
            "Basic " ++ userToken

        request =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Content-Type" "application/json"
                    , Http.header "Authorization" authorization
                    ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson decodeBody
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send messageAfterResponse request


requestFlowUsers : Config -> Cmd Msg
requestFlowUsers config =
    requestFlow
        (config.apiRoot ++ config.flowUrl ++ "/users")
        config.userToken
        decodeUsers
        GetFlowUserResponse


requestFlowMessages : Config -> Cmd Msg
requestFlowMessages config =
    requestFlow
        (config.apiRoot ++ config.flowUrl ++ "/messages?search=jolt&limit=100")
        config.userToken
        decodeMessages
        GetFlowMessagesResponse
