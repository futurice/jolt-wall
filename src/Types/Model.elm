module Types.Model exposing (..)

import Time exposing (Time)
import Types.Message exposing (Message)
import Types.User exposing (User)
import Types.Config exposing (Config)


type alias Model =
    { currentTime : Time
    , flowMessagesLoading : Bool
    , flowMessagesLoadingError : Maybe String
    , flowMessages : List Message
    , flowUsers : List User
    , flowUsersError : Maybe String
    , config : Config
    }
