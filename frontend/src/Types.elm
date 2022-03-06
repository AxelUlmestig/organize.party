module Types exposing (
    State(..),
    PageState,

    Msg(..),
    NewEventMsg(..),

    Event,
    EventInput,
    AttendeeInput,
    Attendee,
    AttendeeStatus(..)
  )

import Browser.Navigation as Nav
import Time as Time
import DurationDatePicker as DP
import Http
import Browser
import Url exposing (Url)

-- State
type State
    = WaitingForInput String
    | Loading
    | Failure
    | ViewEventState Event AttendeeInput
    | NewEventState { picker: DP.DatePicker, input: EventInput }

type alias PageState a = { key: Nav.Key
                         , timeZone : Time.Zone
                         , state: a
                         }

-- Msg
type Msg
    = CreatedEvent (Result Http.Error Event)
    | AttendedEvent (Result Http.Error Event)
    | GetCat String
    | SetId String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | UpdateAttendeeInput AttendeeInput
    | CreateEventMsg EventInput
    | AttendMsg AttendeeInput
    -- | UpdatePicker ( DP.DatePicker, Maybe Time.Posix )
    | CurrentTimeIs Time.Zone Time.Posix
    | NewEventMsg NewEventMsg

type NewEventMsg
    = UpdateEventInput DP.DatePicker EventInput
    | UpdateEventStartTime (DP.DatePicker, Maybe (Time.Posix, Time.Posix))
    | OpenPicker


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

