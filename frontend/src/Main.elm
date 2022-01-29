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
import SingleDatePicker as DP
import Time as Time
import Task as Task
import Iso8601 as Iso8601
import Date as Date

type State
    = WaitingForInput String
    | Loading
    | Failure
    | ViewEventState Event AttendeeInput
    | NewEventState { picker: DP.DatePicker, input: EventInput }

type alias PageState = { key: Nav.Key
                       , timeZone : Time.Zone
                       , state: State
                       }

type alias EventInput =
  { title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Time.Posix
  , location       : String
  -- , googleMapsLink : Maybe String
  }

emptyEventInput : Time.Posix -> Time.Posix -> EventInput
emptyEventInput startTime endTime = { title = ""
                  , description = ""
                  , location = ""
                  , startTime = startTime
                  , endTime = endTime
                  }

encodeEventInput : EventInput -> Value
encodeEventInput { title, description, location, startTime, endTime } = Encode.object
                                                      [ ("title", Encode.string title)
                                                      , ("description", Encode.string description)
                                                      , ("location", Encode.string location)
                                                      , ("startTime", Iso8601.encode startTime)
                                                      , ("endTime", Iso8601.encode endTime)
                                                      ]

type alias AttendeeInput =
  { eventId : String
  , email : String
  , name : String
  , status : AttendeeStatus
  , plusOne : Bool
  }

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
      , ("email", Encode.string email)
      , ("name", Encode.string name)
      , ("status", encodeAttendeeStatus status)
      , ("plusOne", Encode.bool plusOne)
      ]

type alias Event =
  { id : String
  , title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Time.Posix
  , location       : String
  , attendees      : List Attendee
  -- , googleMapsLink : Maybe String
  }

type alias Attendee =
  { name : String
  , status : AttendeeStatus
  , plusOne : Bool
  }

type AttendeeStatus
  = Coming
  | MaybeComing
  | NotComing


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

-- view : State -> Html Msg
view : PageState -> Browser.Document Msg
view state =
  let
    updatePicker : EventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> Msg
    updatePicker input (picker, mTimestamp) = case mTimestamp of
                                                  Just timestamp -> UpdateEventInput picker { input | startTime = timestamp }
                                                  Nothing -> UpdateEventInput picker input

    viewEventDate : Time.Posix -> Time.Posix -> Html Msg
    viewEventDate start end =
        let
            oneDayMillis = 24 * 60 * 60 * 1000
            timeDiff = Time.posixToMillis end - Time.posixToMillis start

            formatTime : Time.Posix -> String
            formatTime time = String.fromInt (Time.toHour state.timeZone start) ++ ":" ++ String.fromInt (Time.toMinute state.timeZone start)

            formatDate : Time.Posix -> String
            formatDate time = Date.toIsoString (Date.fromPosix state.timeZone time)
        in if timeDiff < oneDayMillis
        then H.div [] [ H.text (formatDate start ++ ", " ++ formatTime start ++ " - " ++ formatTime end) ]
        else H.div [] [ H.text (formatDate start ++ " " ++ formatTime start ++ ", " ++ formatDate end ++ " " ++ formatTime end) ]
  in
    Browser.Document "📅" [
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

          NewEventState { picker, input } -> H.div [] [
              H.h3 [] [ H.text "Create A New Event" ]
              , H.div [] [ H.text "Title: ", H.input [ A.value input.title, onInput (\t -> UpdateEventInput picker { input | title = t }) ] [] ]
              , H.div [] [ H.text "Description: ", H.input [ A.value input.description, onInput (\d -> UpdateEventInput picker { input | description = d }) ] [] ]
              , H.div [] [ H.button [ onClick OpenPicker ] [ H.text "click me" ], DP.view  (DP.defaultSettings Time.utc (updatePicker input)) picker ]
              -- , H.div [] [ H.text "Start Time: ", H.input [ A.type_ "datetime-local",  A.value input.startTime, onInput (\t -> UpdateEventInput { input | startTime = t })  ] [] ]
              -- , H.div [] [ H.text "End Time: ", H.input [ A.type_ "datetime-local" ] [] ]
              , H.div [] [ H.text "Location: ", H.input [] [] ]
              , H.button [ onClick (CreateEventMsg input) ] [ H.text "Submit" ]
            ]
    ]


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "http://localhost:8081/api/v1/events/" ++ id
        , expect = Http.expectJson CreatedEvent eventDecoder
        }

createNewEvent : EventInput -> Cmd Msg
createNewEvent input = Http.post
                      { url = "http://localhost:8081/api/v1/events"
                      , expect = Http.expectJson CreatedEvent eventDecoder
                      , body = Http.jsonBody (encodeEventInput input)
                      }

attendEvent : AttendeeInput -> Cmd Msg
attendEvent input = Http.request
                      { method = "PUT"
                      , headers = []
                      , url = "http://localhost:8081/api/v1/events/" ++ input.eventId ++ "/attend"
                      , expect = Http.expectJson AttendedEvent eventDecoder
                      , body = Http.jsonBody (encodeAttendeeInput input)
                      , timeout = Nothing
                      , tracker = Nothing
                      }

init : () -> Url -> Nav.Key -> ( PageState, Cmd Msg )
init _ url key =
  let
    (state, cmd) = case P.parse routeParser url of
                       -- Just NewEvent -> ( Loading, Task.perform CurrentTimeIs Time.now )
                       Just NewEvent -> ( Loading, Task.perform identity (Task.andThen (\zone -> Task.map (CurrentTimeIs zone) Time.now) Time.here))
                       -- Just NewEvent -> ( NewEventState { picker = DP.init, input = emptyEventInput }, Cmd.none )
                       Just (EventId id) -> ( Loading, fetchEvent id )
                       Nothing -> ( Failure, Cmd.none )
  in ( { key = key, timeZone = Time.utc, state = state }, cmd )

type Msg
    = CreatedEvent (Result Http.Error Event)
    | AttendedEvent (Result Http.Error Event)
    | GetCat String
    | SetId String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | UpdateEventInput DP.DatePicker EventInput
    | UpdateAttendeeInput AttendeeInput
    | CreateEventMsg EventInput
    | AttendMsg AttendeeInput
    -- | UpdatePicker ( DP.DatePicker, Maybe Time.Posix )
    | OpenPicker
    | CurrentTimeIs Time.Zone Time.Posix



update : Msg -> PageState -> ( PageState, Cmd Msg )
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
                UpdateEventInput picker input -> (Nothing, NewEventState { picker = picker, input = input }, Cmd.none )
                CreateEventMsg input -> ( Nothing, Loading, createNewEvent input )
                AttendMsg input -> ( Nothing, Loading, attendEvent input )
                CurrentTimeIs zone time -> case state of
                                          -- NewEventState { picker, input } -> ( Loading , createNewEvent { input | startTime = Just time, endTime = Just time } )
                                          Loading -> ( Just zone, NewEventState { picker = DP.init, input = emptyEventInput time time }, Cmd.none )
                                          _ -> ( Just zone, state, Cmd.none)
                OpenPicker -> case state of
                                NewEventState x -> ( Nothing, NewEventState { x | picker = x.picker }, Cmd.none )
                                _ -> ( Nothing, state, Cmd.none )
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

main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
