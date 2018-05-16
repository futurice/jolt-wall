module Main exposing (..)

import Html exposing (Html, text, div, h1)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder)
import Time exposing (Time, second, millisecond)
import Task
import Types.Config exposing (Config)
import Types.Model exposing (Model)
import Types.Message exposing (Message, decodeMessages)
import Types.User exposing (User, decodeUsers)
import Components.JoltMessages exposing (joltMessages)
import Components.JoltCounts exposing (joltCounts)


---- COMMANDS ----


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform Tick


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


type Msg
    = GetFlowJolts
    | GetFlowMessagesResponse (Result Http.Error (List Message))
    | GetFlowUserResponse (Result Http.Error (List User))
    | Tick Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetFlowJolts ->
            ( { model | flowMessagesLoading = True, flowMessagesLoadingError = Nothing }
            , requestFlowMessages model.config
            )

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

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (300 * second) Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        messagesError =
            case model.flowMessagesLoadingError of
                Just error ->
                    div [] [ text error ]

                Nothing ->
                    text ""

        usersError =
            case model.flowUsersError of
                Just error ->
                    div [] [ text error ]

                Nothing ->
                    text ""

        loadingText =
            if model.flowMessagesLoading then
                "   Loading..."
            else
                ""
    in
        div []
            ([ joltCounts model
             , messagesError
             , usersError
             , h1 [ class "jolts-header" ]
                [ text <| "Jolt feed" ++ loadingText
                  --, button [ onClick GetFlowJolts ] [ text "Get jolts" ]
                ]
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
