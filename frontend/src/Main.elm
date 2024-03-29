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
    | NavbarMsg Navbar.NavbarMsg
    -- | DoNothing


type State
    = Loading
    | Failure
    | NewEventState NewEvent.State
    | ViewEventState ViewEvent.ViewEventState
    | EditEventState EditEvent.State
    | AboutState About.State
    | NewForgetMeRequestState NewForgetMeRequest.State
    | ForgetMeRequestState ForgetMeRequest.State


view : PageState Navbar.NavbarState State -> Browser.Document Msg
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
                      [ H.text "Loading..."
                      ]

                Failure ->
                    H.div [ A.class "center" ]
                      [ H.text "Something went wrong, please try again later."
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

mapPageStateAndCmd : (a -> b) -> (c -> d) -> ( PageState navbarState a, Cmd c ) -> ( PageState navbarState b, Cmd d)
mapPageStateAndCmd f g ( pageState, cmd ) = ( mapPageState f pageState, Cmd.map g cmd )

update : Msg -> PageState Navbar.NavbarState State -> ( PageState Navbar.NavbarState State, Cmd Msg )
update msg pageState =
    case (pageState.state, msg) of
        (_, NavbarMsg navbarMsg) ->
            let (newNavbarState, newCmd) = Navbar.update navbarMsg pageState.navbarState
            in ({ pageState | navbarState = newNavbarState }, Cmd.map NavbarMsg newCmd)

        (_, CurrentTimeIs url zone time) ->
            let ( newPageState, newCmd ) =
                    case P.parse routeParser url of
                        Just NewEventR ->
                            let (newEventState, newEventCmd ) = NewEvent.init zone time
                            in ( setPageState (NewEventState newEventState) pageState, Cmd.map NewEventMsg newEventCmd )

                        Just (ViewEventR id) ->
                            let (viewEventState, viewEventCmd) = ViewEvent.init (ViewEvent.ViewEventFromId id) False
                            in ( setPageState (ViewEventState viewEventState) pageState, Cmd.map ViewEventMsg viewEventCmd )

                        Just (EditEventR id) ->
                            let (editEventState, editEventCmd) = EditEvent.init id
                            in ( setPageState (EditEventState editEventState) pageState, Cmd.map EditEventMsg editEventCmd )

                        Just AboutR ->
                            let (aboutState, aboutCmd) = About.init
                            in ( setPageState (AboutState aboutState) pageState, Cmd.map AboutMsg aboutCmd )

                        Just NewForgetMeRequestR ->
                            let (newForgetMeRequestState, newForgetMeRequestCmd) = NewForgetMeRequest.init
                            in ( setPageState (NewForgetMeRequestState newForgetMeRequestState) pageState, Cmd.map NewForgetMeRequestMsg newForgetMeRequestCmd )

                        Just (ForgetMeRequestR requestId) ->
                            let (forgetMeRequestState, forgetMeRequestCmd) = ForgetMeRequest.init requestId
                            in ( setPageState (ForgetMeRequestState forgetMeRequestState) pageState, Cmd.map ForgetMeRequestMsg forgetMeRequestCmd )

                        Nothing ->
                            ( setPageState Failure pageState, Cmd.none )
            in ( { newPageState | timeZone = zone, currentTime = time, pageUrl = url }, newCmd )

        (_, UrlRequest urlRequest) ->
            case urlRequest of
                Browser.Internal url ->
                    ( pageState, Nav.pushUrl pageState.key (Url.toString url) )

                Browser.External href ->
                    ( pageState, Nav.load href )

        (_, UrlChange url) ->
            let ( newPageState, newCmd ) =
                    case (P.parse routeParser url, pageState.state) of
                        (Just NewEventR, NewEventState nes ) ->
                            ( pageState, Cmd.none )

                        (Just NewEventR, _ ) ->
                            let (newEventState, newEventCmd ) = NewEvent.init pageState.timeZone pageState.currentTime
                            in ( setPageState (NewEventState newEventState) pageState, Cmd.map NewEventMsg newEventCmd )

                        -- TODO: what if the event id changes?
                        (Just (ViewEventR id), ViewEventState _) ->
                            ( pageState, Cmd.none )

                        (Just (ViewEventR id), _) ->
                            let (viewEventState, viewEventCmd) = ViewEvent.init (ViewEvent.ViewEventFromId id) False
                            in ( setPageState (ViewEventState viewEventState) pageState, Cmd.map ViewEventMsg viewEventCmd )

                        (Just (EditEventR id), EditEventState _) ->
                            ( pageState, Cmd.none )

                        (Just (EditEventR id), _) ->
                            let (editEventState, editEventCmd) = EditEvent.init id
                            in ( setPageState (EditEventState editEventState) pageState, Cmd.map EditEventMsg editEventCmd )

                        (Just AboutR, AboutState _) ->
                            ( pageState, Cmd.none )

                        (Just AboutR, _) ->
                            let (aboutState, aboutCmd) = About.init
                            in ( setPageState (AboutState aboutState) pageState, Cmd.map AboutMsg aboutCmd )

                        (Just NewForgetMeRequestR, NewForgetMeRequestState _) ->
                            ( pageState, Cmd.none )

                        (Just NewForgetMeRequestR, _) ->
                            let (newForgetMeRequestState, newForgetMeRequestCmd) = NewForgetMeRequest.init
                            in ( setPageState (NewForgetMeRequestState newForgetMeRequestState) pageState, Cmd.map NewForgetMeRequestMsg newForgetMeRequestCmd )

                        (Just (ForgetMeRequestR requestId), ForgetMeRequestState _) ->
                            ( pageState, Cmd.none )

                        (Just (ForgetMeRequestR requestId), _) ->
                            let (forgetMeRequestState, forgetMeRequestCmd) = ForgetMeRequest.init requestId
                            in ( setPageState (ForgetMeRequestState forgetMeRequestState) pageState, Cmd.map ForgetMeRequestMsg forgetMeRequestCmd )

                        (Nothing, _) ->
                            ( setPageState Failure pageState, Cmd.none )
            in ( { newPageState | pageUrl = url }, newCmd )

        (NewEventState nes, NewEventMsg nem) ->
            case nem of
              NewEvent.CreatedEvent event ->
                  let ( newState, newCmd ) = ViewEvent.init (ViewEvent.ViewEventFromEvent event) True
                  in ( setPageState (ViewEventState newState) pageState, Cmd.batch [Cmd.map ViewEventMsg newCmd, Nav.pushUrl pageState.key ("/e/" ++ event.id)] )
              NewEvent.InternalMsg internalMsg ->
                  let ( newState, newCmd ) = NewEvent.update internalMsg (setPageState nes pageState)
                  in ( mapPageState NewEventState newState, Cmd.map NewEventMsg newCmd )

        (ViewEventState ves, ViewEventMsg vem) ->
            case vem of
                -- TODO: this is still an href in ViewEventPage, update it to generate a msg
                ViewEvent.ViewEditEventPage eventId ->
                    let ( newState, newCmd ) = EditEvent.init eventId
                    in ( setPageState (EditEventState newState) pageState, Cmd.batch [Cmd.map EditEventMsg newCmd, Nav.pushUrl pageState.key ("/e/" ++ eventId ++ "/edit")] )
                ViewEvent.InternalMsg internalMsg ->
                    let ( newState, newCmd ) = ViewEvent.update internalMsg (setPageState ves pageState)
                    in ( mapPageState ViewEventState newState, Cmd.map ViewEventMsg newCmd )

        (EditEventState ves, EditEventMsg vem) ->
            case vem of
                EditEvent.EditSuccessful event ->
                  let ( newState, newCmd ) = ViewEvent.init (ViewEvent.ViewEventFromEvent event) False
                  in ( setPageState (ViewEventState newState) pageState, Cmd.batch [Cmd.map ViewEventMsg newCmd, Nav.pushUrl pageState.key ("/e/" ++ event.id)] )
                EditEvent.EditCancelled eventId ->
                  let ( newState, newCmd ) = ViewEvent.init (ViewEvent.ViewEventFromId eventId) False
                  in ( setPageState (ViewEventState newState) pageState, Cmd.batch [Cmd.map ViewEventMsg newCmd, Nav.pushUrl pageState.key ("/e/" ++ eventId)] )
                EditEvent.InternalMsg internalMsg ->
                  let ( newState, newCmd ) = EditEvent.update internalMsg (setPageState ves pageState)
                  in ( mapPageState EditEventState newState, Cmd.map EditEventMsg newCmd )

        (NewForgetMeRequestState nfmrs, NewForgetMeRequestMsg nfmrm) ->
            case nfmrm of
                NewForgetMeRequest.InternalMsg internalMsg ->
                  let ( newState, newCmd ) = NewForgetMeRequest.update internalMsg (setPageState nfmrs pageState)
                  in ( mapPageState NewForgetMeRequestState newState, Cmd.map NewForgetMeRequestMsg newCmd )

        (ForgetMeRequestState fmrs, ForgetMeRequestMsg fmrm) ->
            case fmrm of
                ForgetMeRequest.InternalMsg internalMsg ->
                  let ( newState, newCmd ) = ForgetMeRequest.update internalMsg (setPageState fmrs pageState)
                  in ( mapPageState ForgetMeRequestState newState, Cmd.map ForgetMeRequestMsg newCmd )

        _ -> ( pageState, Cmd.none )


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ P.map NewEventR P.top
        , P.map ViewEventR (s "e" </> string)
        , P.map EditEventR (s "e" </> string </> s "edit")
        , P.map AboutR (s "about")
        , P.map NewForgetMeRequestR (s "forget-me")
        , P.map ForgetMeRequestR (s "forget-me" </> string)
        ]


type Route
    = NewEventR
    | ViewEventR String
    | EditEventR String
    | AboutR
    | NewForgetMeRequestR
    | ForgetMeRequestR String


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

