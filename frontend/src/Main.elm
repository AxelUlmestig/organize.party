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

encodeAttendeeInput : AttendeeInput -> Value
encodeAttendeeInput { eventId, email, name, status, plusOne } =
  let
    encodeAttendeeStatus ai = case ai of
                                Coming -> Encode.string "Coming"
                                MaybeComing -> Encode.string "MaybeComing"
                                NotComing -> Encode.string "NotComing"
  in
    Encode.object
      [ ("eventId", Encode.string eventId)
      , ("email", Encode.string (String.trim(email)))
      , ("name", Encode.string (String.trim(name)))
      , ("status", encodeAttendeeStatus status)
      , ("plusOne", Encode.bool plusOne)
      ]

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
          WaitingForInput eventId ->
              H.div []
              [ H.input [ A.value eventId, onInput SetId ] []
              , H.button [ onClick (GetCat eventId) ] [ H.text "Submit"] ]

          Loading ->
              H.text "loading..."

          Failure ->
              H.text "failed to fetch new cat image"

          ViewEventState {title, description, startTime, endTime, location, attendees} attendeeInput ->
            let
              onStatusUpdate newStatus = case newStatus of
                                          "Coming" -> UpdateAttendeeInput { attendeeInput | status = Coming }
                                          "Maybe Coming" -> UpdateAttendeeInput { attendeeInput | status = MaybeComing }
                                          "Not Coming" -> UpdateAttendeeInput { attendeeInput | status = NotComing }
                                          _ -> UpdateAttendeeInput attendeeInput
            in
              H.div []
                [ H.div []
                    [ H.div [] [ H.h1 [] [ H.text title ] ]
                    , viewEventDate startTime endTime
                    , H.div [] [ H.text location ]
                    , H.div [] [ H.text description ]
                    ]
                , H.br [] []
                , H.div []
                    [ H.b [] [ H.text "Are you attending?" ]
                    , H.div [] [ H.text "email: ", H.input [ A.value attendeeInput.email, onInput (\e -> UpdateAttendeeInput { attendeeInput | email = e }), A.placeholder "Your email" ] [] ]
                    , H.div [] [ H.text "name: ", H.input [ A.value attendeeInput.name, onInput (\fn -> UpdateAttendeeInput { attendeeInput | name = fn }), A.placeholder "Your name" ] [] ]
                    , H.div [] [ H.text "plus one? ", H.input [ A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> UpdateAttendeeInput { attendeeInput | plusOne = po }) ] [] ]
                    , H.div []
                        [ H.select [ onInput onStatusUpdate ]
                            [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                            , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                            , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                            ]
                        ]
                    , H.button [ onClick (AttendMsg attendeeInput) ] [ H.text "Submit" ]
                    ]
                , H.br [] []
                , H.h3 [] [ H.text "Attendees" ]
                , H.table []
                   ( H.tr [] [ H.th [] [ H.text "Name" ], H.th [] [ H.text "Coming?" ], H.th [] [ H.text "Plus One?" ] ]
                   :: (List.map (\{name, status, plusOne} -> H.tr [] [ H.td [] [ H.text name ]
                                                                                    , H.td [] [ H.text (attendeeStatusToString status) ]
                                                                                    , H.td [] [ H.text (if plusOne then "Yes" else "No") ]
                                                                                    ]) attendees)
                   )
                ]

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

attendEvent : AttendeeInput -> Cmd Msg
attendEvent input = Http.request
                      { method = "PUT"
                      , headers = []
                      , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
                      , expect = Http.expectJson AttendedEvent eventDecoder
                      , body = Http.jsonBody (encodeAttendeeInput input)
                      , timeout = Nothing
                      , tracker = Nothing
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
update msg { key, timeZone, state } =
    let
        (mZone, nextState, cmd) =
            case msg of
                SetId id -> ( Nothing, WaitingForInput id, Cmd.none )
                GetCat id -> ( Nothing, Loading, (fetchEvent id) )
                CreatedEvent result ->
                    case result of
                        Ok event ->
                            ( Nothing, ViewEventState event (emptyAttendeeInput event.id), Nav.pushUrl key ("/e/" ++ event.id) )
                        Err _ ->
                            ( Nothing, Failure, Cmd.none )
                AttendedEvent result ->
                    case result of
                        Ok event ->
                            ( Nothing, ViewEventState event (emptyAttendeeInput event.id), Nav.pushUrl key ("/e/" ++ event.id) )
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
                AttendMsg input -> ( Nothing, Loading, attendEvent input )
                CurrentTimeIs zone time -> case state of
                                          Loading -> ( Just zone, NewEventState { picker = DP.init, input = emptyEventInput time time }, Cmd.none )
                                          _ -> ( Just zone, state, Cmd.none)
                UpdateAttendeeInput input -> case state of
                                               ViewEventState event _ -> ( Nothing, ViewEventState event input, Cmd.none )
                                               _ -> ( Nothing, state, Cmd.none )
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
