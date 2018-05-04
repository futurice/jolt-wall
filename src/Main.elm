module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button)
import Html.Attributes exposing (src, disabled, id, class)
import Http
import Json.Decode exposing (Decoder)
import Time exposing (Time, second, millisecond)
import Date exposing (Date, Month, toTime, fromTime)
import Time.DateTime as DateTime exposing (DateTime, fromTimestamp, toTimestamp, toISO8601, addMonths)
import Maybe exposing (withDefault)
import List exposing (take)
import Task
import Types.Message exposing (Message, decodeMessages)
import Types.User exposing (User, decodeUsers)
import Components.JoltMessage exposing (joltMessage)
-- import Helpers.Constants exposing (userToken, apiRoot, flowUrl)

type alias Config =
    { 
      userToken: String,
      apiRoot: String,
      flowUrl: String
    }


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



---- MODEL ----


type alias Model =
    { currentTime : Time
    , flowMessagesLoading : Bool
    , flowMessagesLoadingError : Maybe String
    , flowMessages : List Message
    , flowUsers : List User
    , flowUsersError : Maybe String
    , config: Config
    }


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

        currentDate =
            model.currentTime
                |> Date.fromTime

        currentDateTime =
            model.currentTime
                |> fromTimestamp

        thisYear =
            Date.year currentDate

        thisMonth =
            Date.month currentDate

        previousMonth : Int -> Time
        previousMonth months =
            currentDateTime
                |> addMonths months
                |> toTimestamp

        allMonths : List Time
        allMonths =
            [ previousMonth -1
            , previousMonth -2
            , previousMonth -3
            ]

        validJolts =
            model.flowMessages
                |> List.reverse
                |> List.filter (\item -> item.event == "message")
                |> List.filter (\item -> item.content /= Nothing)

        joltsThisMonth =
            validJolts
                |> List.filter
                    (\jolt ->
                        (Date.month jolt.sent)
                            == thisMonth
                            && (Date.year jolt.sent)
                            == thisYear
                    )
                |> List.length

        joltsInPreviousMonths : List ( Month, Int )
        joltsInPreviousMonths =
            allMonths
                |> List.map
                    (\time ->
                        let
                            year =
                                time
                                    |> fromTime
                                    |> Date.year

                            month =
                                time
                                    |> fromTime
                                    |> Date.month

                            joltAmount =
                                validJolts
                                    |> List.filter
                                        (\jolt ->
                                            (Date.month jolt.sent)
                                                == month
                                                && (Date.year jolt.sent)
                                                == year
                                        )
                                    |> List.length
                        in
                            ( month, joltAmount )
                    )

        renderJoltHistory : List ( Month, Int ) -> List (Html Msg)
        renderJoltHistory monthJoltsList =
            monthJoltsList
                |> List.map
                    (\monthJolts ->
                        let
                            monthString =
                                monthJolts
                                    |> Tuple.first
                                    |> toString

                            joltsString =
                                monthJolts
                                    |> Tuple.second
                                    |> toString
                        in
                            div [ class "jolts-count__history" ]
                                [ div [] [ text monthString ]
                                , div [] [ text joltsString ]
                                ]
                    )

        joltMessages : List (Html Msg)
        joltMessages =
            if List.length model.flowMessages == 0 then
                [ div [ class "no-jolts" ] [ text "KEINE JOLTS!!! Was ist los!" ] ]
            else
                validJolts
                    |> take 12
                    |> List.indexedMap
                        (\index item ->
                            let
                                htmlId =
                                    ("jolt-message-" ++ toString index)
                            in
                                div [ id htmlId, class "jolt-message" ] (joltMessage item model.flowUsers)
                        )
    in
        div []
            ([ div [ class "jolts-counts" ]
                [ div [ class "jolts-count" ]
                    [ div [ class "jolts-count__hero-number" ] [ text <| toString joltsThisMonth ]
                    , div [ class "jolts-count__hero-text" ] [ text "Jolts this month" ]
                    ]
                , div [ class "jolts-count" ] (renderJoltHistory joltsInPreviousMonths)
                , div [ class "munich-logo" ] [ img [ src "/logo.svg", class "munich-logo__image" ] [] ]
                ]
             , messagesError
             , usersError
             , h1 [ class "jolts-header" ]
                [ text <| "Jolt feed" ++ loadingText
                  --, button [ onClick GetFlowJolts ] [ text "Get jolts" ]
                ]
             , div [ class "jolt-messages" ] joltMessages
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
