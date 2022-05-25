module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import Http
import Json.Decode as D -- exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Url exposing (Url)
import Url.Parser as P exposing (Parser, (</>), int, map, oneOf, s, string)
import Url.Parser.Query as Q
import DurationDatePicker as DP
import Time as Time
import Task as Task
import Iso8601 as Iso8601
import Date as Date

import Types exposing (..)
import Page.NewEvent as NewEvent
import Page.ViewEvent as ViewEvent

-- view : State -> Html Msg
view : PageState State -> Browser.Document Msg
view state =
  Browser.Document "📅" [
    H.node "link" [ A.rel "stylesheet", A.href "datepicker.css" ] [],
    case state.state of
      Loading -> H.text "Loading"

      Failure -> H.text "Error"

      ViewEventState viewEventState -> ViewEvent.view (mapPageState (always viewEventState) state)

      NewEventState x -> NewEvent.view (mapPageState (always x) state)
  ]


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (NewEventMsg << CreatedEvent) eventDecoder
        }

init : () -> Url -> Nav.Key -> ( PageState State, Cmd Msg )
init _ url key =
  let
    getCurrentTimeCmd = Task.perform identity (Task.andThen (\zone -> Task.map (CurrentTimeIs url zone) Time.now) Time.here)
    pageState = { key = key, timeZone = Time.utc, state = Loading }
  in ( pageState, getCurrentTimeCmd )

update : Msg -> PageState State -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        { key, timeZone, state } = pageState
        (mZone, nextState, cmd) =
            case msg of
                CurrentTimeIs url zone time ->
                  let
                    (newState, newCmd) = case P.parse routeParser url of
                                       Just NewEventR -> ( NewEventState (NewEvent { picker = DP.init, input = emptyEventInput time time }), Cmd.none )
                                       Just (EventIdR id) -> ( NewEventState NewEventLoading, fetchEvent id )
                                       Nothing -> ( Failure, Cmd.none )
                  in ( Just zone, newState, newCmd )
                UrlRequest _ -> ( Nothing, state, Cmd.none )
                UrlChange _ -> ( Nothing, state, Cmd.none )
                NewEventMsg nem ->
                    case state of
                      NewEventState nes ->
                        let
                          newEventPageState =
                            { state = nes
                            , key = key
                            , timeZone = timeZone
                            }
                          (newState, newCmd) = NewEvent.update nem newEventPageState
                        in (Just newState.timeZone, newState.state, newCmd)
                      _ -> ( Nothing, Loading, Cmd.none )
                ViewEventMsg vem ->
                  case state of
                    ViewEventState viewEventState ->
                      let
                        viewEventPageState = mapPageState (always viewEventState) pageState
                        (newState, newCmd) = ViewEvent.update vem viewEventPageState
                      in (Just newState.timeZone, newState.state, newCmd)
                    _ -> ( Nothing, Loading, Cmd.none )

    in
      ( { key = key, timeZone = Maybe.withDefault timeZone mZone, state = nextState }, cmd )


routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ P.map NewEventR P.top
    , P.map EventIdR (s "e" </> string)
    ]

type Route = NewEventR
           | EventIdR String

pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
  let
    getValueFromPicker : ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
    getValueFromPicker (dp, mTime) = case mTime of
                                             Nothing -> NewEventMsg (UpdateEventInput dp input)
                                             Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput dp { input | startTime = newStart, endTime = newEnd })

  in DP.defaultSettings timeZone getValueFromPicker

subscriptions : PageState State -> Sub Msg
subscriptions model = case model.state of
                        NewEventState (NewEvent { picker, input }) -> DP.subscriptions (pickerSettings model.timeZone picker input) (NewEventMsg << UpdateEventStartTime) picker
                        _ -> Sub.none

main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
