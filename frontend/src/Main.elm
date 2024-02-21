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
import Page.About as About
import Page.NewForgetMeRequest as NewForgetMeRequest
import Page.ForgetMeRequest as ForgetMeRequest
import SingleDatePicker as DP
import Task as Task
import Time as Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Q
import Shared.Navbar as Navbar
import Html

view : PageState State -> Browser.Document Msg
view state =
    Browser.Document "organize.party"
        [ H.node "meta" [ A.name "viewport", A.attribute "content" "width = device-width, initial-scale = 1.0, maximum-scale = 1.0, user-scalable = no" ] []
        , Icon.css
        , Html.map NavbarMsg (Navbar.view state.navbarState)
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

                AboutState x ->
                  H.map AboutMsg <| About.view (mapPageState (always x) state)

                NewForgetMeRequestState x ->
                  H.map NewForgetMeRequestMsg <| NewForgetMeRequest.view (mapPageState (always x) state)

                ForgetMeRequestState x ->
                  H.map ForgetMeRequestMsg <| ForgetMeRequest.view (mapPageState (always x) state)
            ]
        ]


init : () -> Url -> Nav.Key -> ( PageState State, Cmd Msg )
init _ url key =
    let
        getCurrentTimeCmd =
            Task.perform identity (Task.andThen (\zone -> Task.map (CurrentTimeIs url zone) Time.now) Time.here)

        pageState =
            { key = key
            , timeZone = Time.utc
            , currentTime = Time.millisToPosix 0
            , pageUrl = url
            , state = Loading
            , navbarState = Navbar.init
            }
    in
    ( pageState, getCurrentTimeCmd )


update : Msg -> PageState State -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        { key, timeZone, state, pageUrl, currentTime, navbarState } =
            pageState

        packageStateAndCmd newZone newTime nextState cmd =
            ( { key = key, timeZone = newZone, currentTime = newTime, state = nextState, pageUrl = pageUrl, navbarState = navbarState }, cmd )

        packageStateTimeZoneAndCmd nextState zone cmd =
            ( { key = key, timeZone = zone, currentTime = currentTime, state = nextState, pageUrl = pageUrl, navbarState = navbarState }, cmd )

        packageStatePageUrlAndCmd nextState url cmd =
            ( { key = key, timeZone = timeZone, currentTime = currentTime, state = nextState, pageUrl = url, navbarState = navbarState }, cmd )
    in
    case msg of
        DoNothing -> ( pageState, Cmd.none )

        NavbarMsg navbarMsg ->
            let (newState, newCmd) = Navbar.update navbarMsg navbarState
            in ({ pageState | navbarState = newState }, Cmd.map NavbarMsg newCmd)

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

                        Just AboutR ->
                            ( AboutState (), Cmd.none )

                        Just NewForgetMeRequestR ->
                            ( NewForgetMeRequestState (NewForgetMeRequestInputEmail ""), Cmd.none )

                        Just (ForgetMeRequestR requestId) ->
                            ( ForgetMeRequestState (ForgetMeRequestConfirmation requestId), Cmd.none )

                        Nothing ->
                            ( Failure, Cmd.none )
            in
            packageStateAndCmd zone time newState newCmd

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

                ( Just (EditEventR id), EditEventState (EditEvent event Nothing { input }) ) ->
                    if event.id == id then
                        packageStatePageUrlAndCmd state url Cmd.none

                    else
                        packageStatePageUrlAndCmd (EditEventState LoadingEventToEdit) url (EditEvent.fetchEvent id)

                ( Just (EditEventR id), _ ) ->
                    packageStatePageUrlAndCmd (EditEventState LoadingEventToEdit) url (EditEvent.fetchEvent id)

                ( Just AboutR, _ ) ->
                    packageStatePageUrlAndCmd (AboutState ()) url Cmd.none

                ( Just NewForgetMeRequestR, _ ) ->
                    packageStatePageUrlAndCmd (NewForgetMeRequestState (NewForgetMeRequestInputEmail "")) url Cmd.none

                ( Just (ForgetMeRequestR requestId), _ ) ->
                    packageStatePageUrlAndCmd (ForgetMeRequestState (ForgetMeRequestConfirmation requestId)) url Cmd.none

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
                            , currentTime = pageState.currentTime
                            , pageUrl = pageUrl
                            , navbarState = navbarState
                            }

                        ( newState, newCmd ) =
                            NewEvent.update nem newEventPageState
                    in
                    packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

                _ ->
                    packageStateAndCmd timeZone currentTime state Cmd.none

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
                    packageStateAndCmd timeZone currentTime state Cmd.none

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
                    packageStateAndCmd timeZone currentTime state Cmd.none

        AboutMsg am ->
          case state of
            AboutState aboutState ->
              let
                aboutPageState =
                  mapPageState (always aboutState) pageState

                ( newState, newCmd ) =
                  About.update am aboutPageState
              in
              packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

            _ ->
              packageStateAndCmd timeZone currentTime state Cmd.none

        NewForgetMeRequestMsg nfmrm ->
          case state of
            NewForgetMeRequestState newForgetMeRequestState ->
              let
                newForgetMeRequestPageState =
                  mapPageState (always newForgetMeRequestState) pageState

                ( newState, newCmd ) =
                  NewForgetMeRequest.update nfmrm newForgetMeRequestPageState
              in
              packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

            _ ->
              packageStateAndCmd timeZone currentTime state Cmd.none

        ForgetMeRequestMsg fmrm ->
          case state of
            ForgetMeRequestState forgetMeRequestState ->
              let
                forgetMeRequestPageState =
                  mapPageState (always forgetMeRequestState) pageState

                ( newState, newCmd ) =
                  ForgetMeRequest.update fmrm forgetMeRequestPageState
              in
              packageStateTimeZoneAndCmd newState.state newState.timeZone newCmd

            _ ->
              packageStateAndCmd timeZone currentTime state Cmd.none


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ P.map NewEventR P.top
        , P.map ViewEventR (s "e" </> string)
        , P.map EditEventR (s "e" </> string </> s "edit")
        , P.map AboutR (s "about")
        , P.map NewForgetMeRequestR (s "forgetme")
        , P.map ForgetMeRequestR (s "forgetme" </> string)
        ]


type Route
    = NewEventR
    | ViewEventR String
    | EditEventR String
    | AboutR
    | NewForgetMeRequestR
    | ForgetMeRequestR String


subscriptions : PageState State -> Sub Msg
subscriptions model =
    case model.state of
        NewEventState state ->
            NewEvent.handleSubscription (setPageState state model)

        EditEventState state ->
            EditEvent.handleSubscription (setPageState state model)

        ViewEventState state ->
            ViewEvent.handleSubscription (setPageState state model)

        AboutState state ->
            About.handleSubscription (setPageState state model)
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

handleNavbarUpdateResults : PageState State -> ( NavbarState, Cmd NavbarMsg ) -> ( PageState State, Cmd Msg )
handleNavbarUpdateResults pageState ( navbarState, cmd ) =
    ( { pageState | navbarState = navbarState }, Cmd.map NavbarMsg cmd )

