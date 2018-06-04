module Main exposing (..)

import Html exposing (Html, text, div, h1)
import Html.Attributes exposing (class)
import Helpers.Api exposing (requestFlowUsers, requestFlowMessages)
import Time exposing (Time, second)
import Task
import Types.Msg exposing (..)
import Types.Config exposing (Config)
import Types.Model exposing (Model)
import Components.JoltMessages exposing (joltMessages)
import Components.JoltCounts exposing (joltCounts)


---- COMMANDS ----


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform Tick


init : Config -> ( Model, Cmd Msg )
init config =
    ( { currentTime = 0
      , flowMessagesLoading = True
      , flowMessagesLoadingError = Nothing
      , flowMessages = []
      , flowUsers = []
      , flowUsersError = Nothing
      , config = config
      }
    , Cmd.batch
        [ getTime
        , requestFlowUsers config
        ]
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetFlowMessagesResponse (Ok joltMessages) ->
            ( { model | flowMessages = joltMessages, flowMessagesLoading = False }, Cmd.none )

        GetFlowMessagesResponse (Err error) ->
            ( { model | flowMessagesLoadingError = Just (toString error), flowMessagesLoading = False }, Cmd.none )

        GetFlowUserResponse (Ok flowUsers) ->
            ( { model | flowUsers = flowUsers }, Cmd.none )

        GetFlowUserResponse (Err error) ->
            ( { model | flowUsersError = Just (toString error) }, Cmd.none )

        Tick time ->
            ( { model
                | currentTime = time
                , flowMessagesLoading = True
                , flowMessagesLoadingError = Nothing
              }
            , requestFlowMessages model.config
            )

        Tock time ->
            ( { model | flowUsersError = Nothing }
            , requestFlowUsers model.config
            )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (2 * 60 * second) Tick
        , Time.every (60 * 60 * second) Tock
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        renderError errorType error =
            case error of
                Just err ->
                    div [ class "error" ]
                        [ text "Couldn't load "
                        , text errorType
                        , text ": "
                        , text err
                        ]

                Nothing ->
                    text ""

        messagesError =
            renderError "messages" model.flowMessagesLoadingError

        usersError =
            renderError "users" model.flowUsersError

        loadingText =
            if model.flowMessagesLoading then
                div [] [ text "Loading..." ]
            else
                text ""
    in
        div []
            ([ joltCounts model
             , messagesError
             , usersError
             , loadingText
             , joltMessages model
             ]
            )



---- PROGRAM ----


main : Program Config Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
