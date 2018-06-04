module Types.Msg exposing (..)

import Http
import Time exposing (Time)
import Types.Message exposing (Message)
import Types.User exposing (User)


type Msg
    = GetFlowMessagesResponse (Result Http.Error (List Message))
    | GetFlowUserResponse (Result Http.Error (List User))
    | Tick Time
    | Tock Time
    | NoOp
