module Types exposing (
    PageState,
    State(..),
    NewEventState(..),
    ViewEventState(..),

    Msg(..),
    NewEventMsg(..),
    ViewEventMsg(..),

    Event,
    EventInput,
    AttendeeInput,
    Attendee,
    AttendeeStatus(..),

    mapPageState,
    eventDecoder,
    encodeEventInput,
    encodeAttendeeInput,
    emptyAttendeeInput,
    emptyEventInput,
    attendeeStatusToString
  )

import Json.Decode as D
import Browser.Navigation as Nav
import Time as Time
import DurationDatePicker as DP
import Http
import Browser
import Url exposing (Url)
import Iso8601 as Iso8601
import Json.Encode as Encode exposing (Value)

-- State
type State
  = Loading
  | Failure
  | ViewEventState ViewEventState
  | NewEventState NewEventState

type ViewEventState
  = ViewEvent Event AttendeeInput
  | AttendEventLoading

type NewEventState
  = NewEvent { picker: DP.DatePicker, input: EventInput }
  | NewEventLoading

type alias PageState a = { key: Nav.Key
                         , timeZone : Time.Zone
                         , state: a
                         }

-- Msg
type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url
    -- | UpdatePicker ( DP.DatePicker, Maybe Time.Posix )
    | CurrentTimeIs Url Time.Zone Time.Posix
    | NewEventMsg NewEventMsg
    | ViewEventMsg ViewEventMsg

type NewEventMsg
    = UpdateEventInput DP.DatePicker EventInput
    | UpdateEventStartTime (DP.DatePicker, Maybe (Time.Posix, Time.Posix))
    | OpenPicker
    | CreateEventMsg EventInput
    | CreatedEvent (Result Http.Error Event)

type ViewEventMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput


-- Event
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

type alias EventInput =
  { title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Time.Posix
  , location       : String
  -- , googleMapsLink : Maybe String
  }

-- Attendee
type alias Attendee =
  { name : String
  , status : AttendeeStatus
  , plusOne : Bool
  }

type alias AttendeeInput =
  { eventId : String
  , email : String
  , name : String
  , status : AttendeeStatus
  , plusOne : Bool
  }

type AttendeeStatus
  = Coming
  | MaybeComing
  | NotComing

mapPageState : (a -> b) -> PageState a -> PageState b
mapPageState f ps =
  { state = f ps.state
  , timeZone = ps.timeZone
  , key = ps.key
  }

-- encoders and decoders
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

