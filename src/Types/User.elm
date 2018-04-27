module Types.User exposing (..)

import Json.Decode exposing (Decoder, string, int, maybe, float, succeed, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional)


---- TYPE ----


type alias User =
    { id : String
    , nick : String
    , avatar : String
    }



---- DECODERS ----


decodeUsers : Decoder (List User)
decodeUsers =
    Json.Decode.list decodeUser


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "id"
            (float
                |> andThen (succeed << toString)
            )
        |> required "nick" string
        |> required "avatar" string
