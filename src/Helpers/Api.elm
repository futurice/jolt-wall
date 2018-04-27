module Helpers.Api exposing (..)

import Json.Decode exposing (Decoder)
import Http exposing (header, emptyBody, expectJson, request)
import Types.Message exposing (decodeMessages)
import Types.User exposing (decodeUsers)


requestFlow : String -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
requestFlow url decodeBody messageAfterResponse =
    let
        authorization =
            "Basic NjE4NDVkZDhjYTA5ZjJkMmU3MzYxYzM0Yzg5YmZmNTA="

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


requestFlowUsers : Cmd msg
requestFlowUsers msg =
    requestFlow
        "https://api.flowdock.com/flows/futurice/munich/users"
        decodeUsers
        msg


requestFlowMessages : Cmd msg
requestFlowMessages msg =
    requestFlow
        "https://api.flowdock.com/flows/futurice/munich/messages?search=jolt&limit=100"
        decodeMessages
        msg
