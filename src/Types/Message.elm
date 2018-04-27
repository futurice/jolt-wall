module Types.Message exposing (..)

import Date exposing (Date, fromTime)
import Json.Decode exposing (Decoder, string, int, maybe, float, succeed, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional)


---- TYPE ----


type alias Message =
    { user : String
    , content : Maybe String
    , event : String
    , uuid : String
    , sent : Date
    , createdAt : String
    }



---- DECODERS ----


decodeDate : Decoder Date
decodeDate =
    float
        |> andThen (succeed << Date.fromTime)


decodeMessages : Decoder (List Message)
decodeMessages =
    Json.Decode.list decodeMessage


decodeMessage : Decoder Message
decodeMessage =
    decode Message
        |> required "user" string
        |> optional "content" (maybe string) Nothing
        |> required "event" string
        |> required "uuid" string
        |> required "sent" decodeDate
        |> required "created_at" string
