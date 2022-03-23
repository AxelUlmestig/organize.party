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
import NewEvent
import ViewEvent

encodeEventInput : EventInput -> Value
encodeEventInput { title, description, location, startTime, endTime } = Encode.object
                                                      [ ("title", Encode.string title)
                                                      , ("description", Encode.string description)
                                                      , ("location", Encode.string location)
                                                      , ("startTime", Iso8601.encode startTime)
                                                      , ("endTime", Iso8601.encode endTime)
                                                      ]

emptyAttendeeInput : String -> AttendeeInput
emptyAttendeeInput eventId =
  { eventId = eventId
  , email = ""
  , name = ""
  , status = Coming
  , plusOne = False
  }

eventDecoder : D.Decoder Event
eventDecoder = D.map7 Event
                 (D.field "id" D.string)
                 (D.field "title" D.string)
                 (D.field "description" D.string)
                 (D.field "startTime" Iso8601.decoder)
                 (D.field "endTime" Iso8601.decoder)
                 (D.field "location" D.string)
                 (D.field "attendees" (D.list attendeeDecoder))

attendeeDecoder : D.Decoder Attendee
attendeeDecoder = D.map3 Attendee
                    (D.field "name" D.string)
                    (D.field "status" attendeeStatusDecoder)
                    (D.field "plusOne" D.bool)

attendeeStatusDecoder : D.Decoder AttendeeStatus
attendeeStatusDecoder =
  D.string
    |> D.andThen (\str ->
        case str of
          "Coming" -> D.succeed Coming
          "MaybeComing" -> D.succeed MaybeComing
          "NotComing" -> D.succeed NotComing
          somethingElse -> D.fail ("Unknown status: " ++ somethingElse)
      )

attendeeStatusToString : AttendeeStatus -> String
attendeeStatusToString status = case status of
                                  Coming -> "Coming"
                                  MaybeComing -> "Maybe Coming"
                                  NotComing -> "Not Coming"

emptyEventInput : Time.Posix -> Time.Posix -> EventInput
emptyEventInput startTime endTime = { title = ""
                  , description = ""
                  , location = ""
                  , startTime = startTime
                  , endTime = endTime
                  }

-- view : State -> Html Msg
view : PageState State -> Browser.Document Msg
view state =
  let
    updatePicker : EventInput -> ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
    updatePicker input (picker, mTimestamp) = case mTimestamp of
                                                  Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput picker { input | startTime = newStart, endTime = newEnd })
                                                  Nothing -> NewEventMsg (UpdateEventInput picker input)

    viewEventDate : Time.Posix -> Time.Posix -> Html Msg
    viewEventDate start end =
        let
            oneDayMillis = 24 * 60 * 60 * 1000
            timeDiff = Time.posixToMillis end - Time.posixToMillis start

            formatTime : Time.Posix -> String
            formatTime time = String.fromInt (Time.toHour state.timeZone time) ++ ":" ++ String.fromInt (Time.toMinute state.timeZone time)

            formatDate : Time.Posix -> String
            formatDate time = Date.toIsoString (Date.fromPosix state.timeZone time)
        in if timeDiff < oneDayMillis
        then H.div [] [ H.text (formatDate start ++ ", " ++ formatTime start ++ " - " ++ formatTime end) ]
        else H.div [] [ H.text (formatDate start ++ " " ++ formatTime start ++ ", " ++ formatDate end ++ " " ++ formatTime end) ]
  in
    Browser.Document "📅" [
      H.node "link" [ A.rel "stylesheet", A.href "datepicker.css" ] [],
      case state.state of
          Loading ->
              H.text "loading..."

          Failure ->
              H.text "Error"

          ViewEventState viewEventState -> ViewEvent.view (mapPageState (always viewEventState) state)

          NewEventState x -> NewEvent.view x
    ]


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson CreatedEvent eventDecoder
        }

createNewEvent : EventInput -> Cmd Msg
createNewEvent input = Http.post
                      { url = "/api/v1/events"
                      , expect = Http.expectJson CreatedEvent eventDecoder
                      , body = Http.jsonBody (encodeEventInput input)
                      }

init : () -> Url -> Nav.Key -> ( PageState State, Cmd Msg )
init _ url key =
  let
    (state, cmd) = case P.parse routeParser url of
                       Just NewEvent -> ( Loading, Task.perform identity (Task.andThen (\zone -> Task.map (CurrentTimeIs zone) Time.now) Time.here) )
                       Just (EventId id) -> ( Loading, fetchEvent id )
                       Nothing -> ( Failure, Cmd.none )
  in ( { key = key, timeZone = Time.utc, state = state }, cmd )

update : Msg -> PageState State -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        { key, timeZone, state } = pageState
        (mZone, nextState, cmd) =
            case msg of
                CreatedEvent result ->
                    case result of
                        Ok event ->
                            ( Nothing, ViewEventState (ViewEvent event (emptyAttendeeInput event.id)), Nav.pushUrl key ("/e/" ++ event.id) )
                        Err _ ->
                            ( Nothing, Failure, Cmd.none )
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
                CreateEventMsg input -> ( Nothing, Loading, createNewEvent input )
                CurrentTimeIs zone time -> case state of
                                          Loading -> ( Just zone, NewEventState { picker = DP.init, input = emptyEventInput time time }, Cmd.none )
                                          _ -> ( Just zone, state, Cmd.none)
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
    [ P.map NewEvent P.top
    , P.map EventId (s "e" </> string)
    ]

type Route = NewEvent
           | EventId String

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
                        NewEventState { picker, input } -> DP.subscriptions (pickerSettings model.timeZone picker input) (NewEventMsg << UpdateEventStartTime) picker
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
