module Main exposing (main)

-- exposing (Decoder, field, string)

import Browser
import Browser.Navigation as Nav
import Date as Date
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601 as Iso8601
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import Page.EditEvent as EditEvent
import Page.NewEvent as NewEvent
import Page.ViewEvent as ViewEvent
import SingleDatePicker as DP
import TimePicker as TP
import Task as Task
import Time as Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Q


view : PageState State -> Browser.Document Msg
view state =
    Browser.Document "📅"
        [ H.node "meta" [ A.name "viewport", A.attribute "content" "width = device-width, initial-scale = 1.0, maximum-scale = 1.0, user-scalable = no" ] []
        , H.node "link" [ A.rel "stylesheet", A.href "/datepicker.css", A.href "/elm-time-picker.css" ] []
        , H.node "link"
            [ A.rel "stylesheet"
            , A.href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
            , A.attribute "integrity" "sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor"
            , A.attribute "crossorigin" "anonymous"
            ]
            []
        , Icon.css
        , H.node "style"
            []
            [ H.text
                """
          body {
            background-color: #fbfafa;
            color: #1c2c3b;
          }

          .modal {
            position: fixed;
            top: 0;
            left: 0;
            height: 100%;
            width: 100%;
            background-color: rgba(0,0,0,.5);
            display: flex;
            justify-content: center;
            align-items: center;
          }

          .modal_window {
            position: relative;
            background-color: white;
            padding: 4em 2em;
            border-radius: 5px;
          }

          .center {
              right: 50%;
              bottom: 50%;
              transform: translate(50%,50%);
              position: absolute;
          }
          """
            ]
        , H.div
            [ A.class "container"
            , A.style "max-width" "700px"
            ]
            [ case state.state of
                Loading ->
                    H.div [ A.class "center" ]
                      [ H.text "Loading"
                      ]

                Failure ->
                    H.div [ A.class "center" ]
                      [ H.text "Error"
                      ]

                ViewEventState viewEventState ->
                    ViewEvent.view (mapPageState (always viewEventState) state)

                NewEventState x ->
                    NewEvent.view (mapPageState (always x) state)

                EditEventState x ->
                    EditEvent.view (mapPageState (always x) state)
            ]
        ]


init : () -> Url -> Nav.Key -> ( PageState State, Cmd Msg )
init _ url key =
    let
        getCurrentTimeCmd =
            Task.perform identity (Task.andThen (\zone -> Task.map (CurrentTimeIs url zone) Time.now) Time.here)

        pageState =
            { key = key, timeZone = Time.utc, pageUrl = url, state = Loading }
    in
    ( pageState, getCurrentTimeCmd )


update : Msg -> PageState State -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        { key, timeZone, state, pageUrl } =
            pageState

        packageStateAndCmd newZone nextState cmd =
            ( { key = key, timeZone = newZone, state = nextState, pageUrl = pageUrl }, cmd )

        packageStateTimeZoneAndCmd nextState zone cmd =
            ( { key = key, timeZone = zone, state = nextState, pageUrl = pageUrl }, cmd )

        packageStatePageUrlAndCmd nextState url cmd =
            ( { key = key, timeZone = timeZone, state = nextState, pageUrl = url }, cmd )
    in
    case msg of
        CurrentTimeIs url zone time ->
            let
                ( newState, newCmd ) =
                    case P.parse routeParser url of
                        Just NewEventR ->
                            ( NewEventState (NewEvent { datePicker = DP.init, timePicker = TP.init Nothing, input = emptyEventInput time }), Cmd.none )

                        Just (ViewEventR id) ->
                            ( ViewEventState LoadingEvent, ViewEvent.fetchEvent id )

                        Just (EditEventR id) ->
                            ( EditEventState LoadingEventToEdit, EditEvent.fetchEvent id )

                        Nothing ->
                            ( Failure, Cmd.none )
            in
            packageStateAndCmd zone newState newCmd

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( pageState, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( pageState, Nav.load href )

        UrlChange url ->
            case ( P.parse routeParser url, state ) of
                ( Just NewEventR, NewEventState nes ) ->
                    packageStatePageUrlAndCmd state url Cmd.none

                ( Just NewEventR, _ ) ->
                    let
                        ( loading, currentTimeIs ) =
                            init () url key
                    in
                    packageStatePageUrlAndCmd loading.state url currentTimeIs

                ( Just (ViewEventR id), ViewEventState AttendEventLoading ) ->
                    packageStatePageUrlAndCmd (ViewEventState LoadingEvent) url (ViewEvent.fetchEvent id)

                ( Just (ViewEventR id), ViewEventState (ViewEvent _ event _) ) ->
                    if event.id == id then
                        packageStatePageUrlAndCmd state url Cmd.none

                    else
                        packageStatePageUrlAndCmd (NewEventState NewEventLoading) url (ViewEvent.fetchEvent id)

                ( Just (ViewEventR id), _ ) ->
                    packageStatePageUrlAndCmd (ViewEventState LoadingEvent) url (ViewEvent.fetchEvent id)

                ( Just (EditEventR id), EditEventState (EditEvent _ Nothing { input }) ) ->
                    if input.id == id then
                        packageStatePageUrlAndCmd state url Cmd.none

                    else
                        packageStatePageUrlAndCmd (EditEventState LoadingEventToEdit) url (EditEvent.fetchEvent id)

                ( Just (EditEventR id), _ ) ->
                    packageStatePageUrlAndCmd (EditEventState LoadingEventToEdit) url (EditEvent.fetchEvent id)

                ( Nothing, _ ) ->
                    packageStatePageUrlAndCmd Failure url Cmd.none

        NewEventMsg nem ->
            case state of
                NewEventState nes ->
                    let
                        newEventPageState =
                            { state = nes
                            , key = key
                            , timeZone = timeZone
                            , pageUrl = pageUrl
                            }

                        ( newState, newCmd ) =
                            NewEvent.update nem newEventPageState
                    in
                    packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

                _ ->
                    packageStateAndCmd timeZone Loading Cmd.none

        ViewEventMsg vem ->
            case state of
                ViewEventState viewEventState ->
                    let
                        viewEventPageState =
                            mapPageState (always viewEventState) pageState

                        ( newState, newCmd ) =
                            ViewEvent.update vem viewEventPageState
                    in
                    packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

                _ ->
                    packageStateAndCmd timeZone Loading Cmd.none

        EditEventMsg eem ->
            case state of
                EditEventState editEventState ->
                    let
                        editEventPageState =
                            mapPageState (always editEventState) pageState

                        ( newState, newCmd ) =
                            EditEvent.update eem editEventPageState
                    in
                    packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

                _ ->
                    packageStateAndCmd timeZone Loading Cmd.none


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ P.map NewEventR P.top
        , P.map ViewEventR (s "e" </> string)
        , P.map EditEventR (s "e" </> string </> s "edit")
        ]


type Route
    = NewEventR
    | ViewEventR String
    | EditEventR String


subscriptions : PageState State -> Sub Msg
subscriptions model =
    case model.state of
        NewEventState state ->
            NewEvent.handleSubscription (setPageState state model)

        EditEventState state ->
            EditEvent.handleSubscription (setPageState state model)

        _ ->
            Sub.none


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
