module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import Page.About as About
import Page.EditEvent as EditEvent
import Page.ForgetMeRequest as ForgetMeRequest
import Page.NewEvent as NewEvent
import Page.NewForgetMeRequest as NewForgetMeRequest
import Page.Unsubscribe as Unsubscribe
import Page.ViewEvent as ViewEvent
import Shared.Navbar as Navbar
import Shared.PageState exposing (PageState, bimapPsCmd, mapPageState, setPageState)
import SingleDatePicker as DP
import Task
import Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Q


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url
    | CurrentTimeIs Url Time.Zone Time.Posix
    | NewEventMsg NewEvent.Msg
    | ViewEventMsg ViewEvent.ViewEventMsg
    | EditEventMsg EditEvent.Msg
    | AboutMsg About.Msg
    | NewForgetMeRequestMsg NewForgetMeRequest.Msg
    | ForgetMeRequestMsg ForgetMeRequest.Msg
    | UnsubscribeMsg Unsubscribe.Msg
    | NavbarMsg Navbar.NavbarMsg


type State
    = Loading
    | Failure
    | NewEventState NewEvent.State
    | ViewEventState ViewEvent.ViewEventState
    | EditEventState EditEvent.State
    | AboutState About.State
    | NewForgetMeRequestState NewForgetMeRequest.State
    | ForgetMeRequestState ForgetMeRequest.State
    | UnsubscribeState Unsubscribe.State


view : PageState Navbar.NavbarState State -> Browser.Document Msg
view state =
    Browser.Document "organize.party"
        [ H.node "meta" [ A.name "viewport", A.attribute "content" "width = device-width, initial-scale = 1.0, maximum-scale = 1.0, user-scalable = no" ] []
        , Icon.css
        , H.map NavbarMsg (Navbar.view state.navbarState)
        , H.div
            [ A.class "container"
            , A.style "max-width" "700px"
            ]
            [ case state.state of
                Loading ->
                    H.div [ A.class "center" ]
                        [ H.text "Loading..."
                        ]

                Failure ->
                    H.div [ A.class "center" ]
                        [ H.text "Something went wrong, please try again later."
                        ]

                ViewEventState viewEventState ->
                    H.map ViewEventMsg <| ViewEvent.view (setPageState viewEventState state)

                NewEventState newEventState ->
                    H.map NewEventMsg <| NewEvent.view (setPageState newEventState state)

                EditEventState x ->
                    H.map EditEventMsg <| EditEvent.view (setPageState x state)

                AboutState x ->
                    H.map AboutMsg <| About.view (setPageState x state)

                NewForgetMeRequestState x ->
                    H.map NewForgetMeRequestMsg <| NewForgetMeRequest.view (setPageState x state)

                ForgetMeRequestState x ->
                    H.map ForgetMeRequestMsg <| ForgetMeRequest.view (setPageState x state)

                UnsubscribeState x ->
                    H.map UnsubscribeMsg <| Unsubscribe.view (setPageState x state)
            ]
        ]


init : () -> Url -> Nav.Key -> ( PageState Navbar.NavbarState State, Cmd Msg )
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


update : Msg -> PageState Navbar.NavbarState State -> ( PageState Navbar.NavbarState State, Cmd Msg )
update msg pageState =
    case ( pageState.state, msg ) of
        ( _, NavbarMsg navbarMsg ) ->
            let
                ( newNavbarState, newCmd ) =
                    Navbar.update navbarMsg pageState.navbarState
            in
            ( { pageState | navbarState = newNavbarState }, Cmd.map NavbarMsg newCmd )

        ( _, CurrentTimeIs url zone time ) ->
            let
                ( newPageState, newCmd ) =
                    case P.parse routeParser url of
                        Just NewEventR ->
                            wrapInit pageState NewEventState NewEventMsg Nothing <| NewEvent.init zone time

                        Just (ViewEventR id) ->
                            wrapInit pageState ViewEventState ViewEventMsg Nothing <| ViewEvent.init (ViewEvent.ViewEventFromId id) False

                        Just (EditEventR id) ->
                            wrapInit pageState EditEventState EditEventMsg Nothing <| EditEvent.init id

                        Just AboutR ->
                            wrapInit pageState AboutState AboutMsg Nothing <| About.init

                        Just NewForgetMeRequestR ->
                            wrapInit pageState NewForgetMeRequestState NewForgetMeRequestMsg Nothing <| NewForgetMeRequest.init

                        Just (ForgetMeRequestR requestId) ->
                            wrapInit pageState ForgetMeRequestState ForgetMeRequestMsg Nothing <| ForgetMeRequest.init requestId

                        Just (UnsubscribeR unsubscribeId) ->
                            wrapInit pageState UnsubscribeState UnsubscribeMsg Nothing <| Unsubscribe.init unsubscribeId

                        Nothing ->
                            ( setPageState Failure pageState, Cmd.none )
            in
            ( { newPageState | timeZone = zone, currentTime = time, pageUrl = url }, newCmd )

        ( _, UrlRequest urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( pageState, Nav.pushUrl pageState.key (Url.toString url) )

                Browser.External href ->
                    ( pageState, Nav.load href )

        ( _, UrlChange url ) ->
            let
                ( newPageState, newCmd ) =
                    case ( P.parse routeParser url, pageState.state ) of
                        ( Just NewEventR, NewEventState nes ) ->
                            ( pageState, Cmd.none )

                        ( Just NewEventR, _ ) ->
                            wrapInit pageState NewEventState NewEventMsg Nothing <| NewEvent.init pageState.timeZone pageState.currentTime

                        -- TODO: what if the event id changes?
                        ( Just (ViewEventR id), ViewEventState _ ) ->
                            ( pageState, Cmd.none )

                        ( Just (ViewEventR id), _ ) ->
                            wrapInit pageState ViewEventState ViewEventMsg Nothing <| ViewEvent.init (ViewEvent.ViewEventFromId id) False

                        ( Just (EditEventR id), EditEventState _ ) ->
                            ( pageState, Cmd.none )

                        ( Just (EditEventR id), _ ) ->
                            wrapInit pageState EditEventState EditEventMsg Nothing <| EditEvent.init id

                        ( Just AboutR, AboutState _ ) ->
                            ( pageState, Cmd.none )

                        ( Just AboutR, _ ) ->
                            wrapInit pageState AboutState AboutMsg Nothing <| About.init

                        ( Just NewForgetMeRequestR, NewForgetMeRequestState _ ) ->
                            ( pageState, Cmd.none )

                        ( Just NewForgetMeRequestR, _ ) ->
                            wrapInit pageState NewForgetMeRequestState NewForgetMeRequestMsg Nothing <| NewForgetMeRequest.init

                        ( Just (ForgetMeRequestR requestId), ForgetMeRequestState _ ) ->
                            ( pageState, Cmd.none )

                        ( Just (ForgetMeRequestR requestId), _ ) ->
                            wrapInit pageState ForgetMeRequestState ForgetMeRequestMsg Nothing <| ForgetMeRequest.init requestId

                        ( Just (UnsubscribeR unsubscribeId), _ ) ->
                            wrapInit pageState UnsubscribeState UnsubscribeMsg Nothing <| Unsubscribe.init unsubscribeId

                        ( Nothing, _ ) ->
                            ( setPageState Failure pageState, Cmd.none )
            in
            ( { newPageState | pageUrl = url }, newCmd )

        ( NewEventState nes, NewEventMsg nem ) ->
            case nem of
                NewEvent.CreatedEvent event ->
                    wrapInit pageState ViewEventState ViewEventMsg (Just ("/e/" ++ event.id)) <| ViewEvent.init (ViewEvent.ViewEventFromEvent event) True

                NewEvent.InternalMsg internalMsg ->
                    bimapPsCmd NewEventState NewEventMsg <| NewEvent.update internalMsg (setPageState nes pageState)

        ( ViewEventState ves, ViewEventMsg vem ) ->
            case vem of
                -- TODO: this is still an href in ViewEventPage, update it to generate a msg
                ViewEvent.ViewEditEventPage eventId ->
                    wrapInit pageState EditEventState EditEventMsg (Just ("/e/" ++ eventId ++ "/edit")) <| EditEvent.init eventId

                ViewEvent.InternalMsg internalMsg ->
                    bimapPsCmd ViewEventState ViewEventMsg <| ViewEvent.update internalMsg (setPageState ves pageState)

        ( EditEventState ves, EditEventMsg vem ) ->
            case vem of
                EditEvent.EditSuccessful event ->
                    wrapInit pageState ViewEventState ViewEventMsg (Just ("/e/" ++ event.id)) <| ViewEvent.init (ViewEvent.ViewEventFromEvent event) False

                EditEvent.EditCancelled eventId ->
                    wrapInit pageState ViewEventState ViewEventMsg (Just ("/e/" ++ eventId)) <| ViewEvent.init (ViewEvent.ViewEventFromId eventId) False

                EditEvent.InternalMsg internalMsg ->
                    bimapPsCmd EditEventState EditEventMsg <| EditEvent.update internalMsg (setPageState ves pageState)

        ( NewForgetMeRequestState nfmrs, NewForgetMeRequestMsg nfmrm ) ->
            case nfmrm of
                NewForgetMeRequest.InternalMsg internalMsg ->
                    bimapPsCmd NewForgetMeRequestState NewForgetMeRequestMsg <| NewForgetMeRequest.update internalMsg (setPageState nfmrs pageState)

        ( ForgetMeRequestState fmrs, ForgetMeRequestMsg fmrm ) ->
            case fmrm of
                ForgetMeRequest.InternalMsg internalMsg ->
                    bimapPsCmd ForgetMeRequestState ForgetMeRequestMsg <| ForgetMeRequest.update internalMsg (setPageState fmrs pageState)

        ( UnsubscribeState us, UnsubscribeMsg um ) ->
            case um of
                Unsubscribe.InternalMsg internalMsg ->
                    bimapPsCmd UnsubscribeState UnsubscribeMsg <| Unsubscribe.update internalMsg (setPageState us pageState)

        ( AboutState fmrs, AboutMsg fmrm ) ->
            case fmrm of
                About.InternalMsg internalMsg ->
                    bimapPsCmd AboutState AboutMsg <| About.update internalMsg (setPageState fmrs pageState)

        _ ->
            ( pageState, Cmd.none )


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ P.map NewEventR P.top
        , P.map ViewEventR (s "e" </> string)
        , P.map EditEventR (s "e" </> string </> s "edit")
        , P.map AboutR (s "about")
        , P.map NewForgetMeRequestR (s "forget-me")
        , P.map ForgetMeRequestR (s "forget-me" </> string)
        , P.map UnsubscribeR (s "unsubscribe" </> string)
        ]


type Route
    = NewEventR
    | ViewEventR String
    | EditEventR String
    | AboutR
    | NewForgetMeRequestR
    | ForgetMeRequestR String
    | UnsubscribeR String


subscriptions : PageState Navbar.NavbarState State -> Sub Msg
subscriptions model =
    case model.state of
        NewEventState state ->
            Sub.map NewEventMsg <| NewEvent.handleSubscription (setPageState state model)

        EditEventState state ->
            Sub.map EditEventMsg <| EditEvent.handleSubscription (setPageState state model)

        ViewEventState state ->
            Sub.map ViewEventMsg <| ViewEvent.handleSubscription (setPageState state model)

        AboutState state ->
            Sub.map AboutMsg <| About.handleSubscription (setPageState state model)

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


handleNavbarUpdateResults : PageState Navbar.NavbarState State -> ( Navbar.NavbarState, Cmd Navbar.NavbarMsg ) -> ( PageState Navbar.NavbarState State, Cmd Msg )
handleNavbarUpdateResults pageState ( navbarState, cmd ) =
    ( { pageState | navbarState = navbarState }, Cmd.map NavbarMsg cmd )


wrapInit : PageState navbar x -> (s1 -> s2) -> (c1 -> c2) -> Maybe String -> ( s1, Cmd c1 ) -> ( PageState navbar s2, Cmd c2 )
wrapInit pageState f g mUrl ( state, cmd ) =
    let
        pushUrlCmd =
            case mUrl of
                Nothing ->
                    Cmd.none

                Just url ->
                    Nav.pushUrl pageState.key url
    in
    ( setPageState (f state) pageState, Cmd.batch [ Cmd.map g cmd, pushUrlCmd ] )
