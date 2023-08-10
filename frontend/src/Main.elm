module Main exposing (main)

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
import Task as Task
import Time as Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Q

view : PageState State -> Browser.Document Msg
view state =
    Browser.Document "organize.party"
        [ H.node "meta" [ A.name "viewport", A.attribute "content" "width = device-width, initial-scale = 1.0, maximum-scale = 1.0, user-scalable = no" ] []
        , Icon.css
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
                    H.map ViewEventMsg <| ViewEvent.view (mapPageState (always viewEventState) state)

                NewEventState x ->
                    H.map NewEventMsg <| NewEvent.view (mapPageState (always x) state)

                EditEventState x ->
                    H.map EditEventMsg <| EditEvent.view (mapPageState (always x) state)
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
        DoNothing -> ( pageState, Cmd.none )

        CurrentTimeIs url zone time ->
            let
                ( newState, newCmd ) =
                    case P.parse routeParser url of
                        Just NewEventR ->
                            ( NewEventState (NewEvent { timezone = zone, picker = DP.init, input = emptyEventInput time }), Cmd.none )

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

                ( Just (EditEventR id), EditEventState (EditEvent eventId _ Nothing { input }) ) ->
                    if eventId == id then
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

        ViewEventState state ->
            ViewEvent.handleSubscription (setPageState state model)

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
