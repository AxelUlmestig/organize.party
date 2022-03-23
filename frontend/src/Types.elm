module Types exposing (
    PageState,
    State(..),
    ViewEventState(..),

    Msg(..),
    NewEventMsg(..),
    ViewEventMsg(..),

    Event,
    EventInput,
    AttendeeInput,
    Attendee,
    AttendeeStatus(..),

    mapPageState
  )

import Browser.Navigation as Nav
import Time as Time
import DurationDatePicker as DP
import Http
import Browser
import Url exposing (Url)

-- State
type State
  = Loading
  | Failure
  | ViewEventState ViewEventState
  | NewEventState { picker: DP.DatePicker, input: EventInput }

type ViewEventState
  = ViewEvent Event AttendeeInput
  | AttendEventLoading

type alias PageState a = { key: Nav.Key
                         , timeZone : Time.Zone
                         , state: a
                         }

-- Msg
type Msg
    = CreatedEvent (Result Http.Error Event)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | CreateEventMsg EventInput
    -- | UpdatePicker ( DP.DatePicker, Maybe Time.Posix )
    | CurrentTimeIs Time.Zone Time.Posix
    | NewEventMsg NewEventMsg
    | ViewEventMsg ViewEventMsg

type NewEventMsg
    = UpdateEventInput DP.DatePicker EventInput
    | UpdateEventStartTime (DP.DatePicker, Maybe (Time.Posix, Time.Posix))
    | OpenPicker

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
