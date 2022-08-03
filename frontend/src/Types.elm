module Types exposing (
    PageState,
    State(..),
    NewEventState(..),
    ViewEventState(..),
    ViewEventStateModal(..),
    EditEventState(..),

    Msg(..),
    NewEventMsg(..),
    ViewEventMsg(..),
    EditEventMsg(..),

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
import SingleDatePicker as DP
import Http
import Browser
import Url exposing (Url)
import Iso8601 as Iso8601
import Json.Encode as Encode exposing (Value)

-- State
type State
  = Loading
  | Failure
  | NewEventState NewEventState
  | ViewEventState ViewEventState
  | EditEventState EditEventState

type NewEventState
  = NewEvent { picker: DP.DatePicker, input: EventInput }
  | NewEventLoading

type ViewEventState
  = ViewEvent (Maybe ViewEventStateModal) Event AttendeeInput
  | AttendEventLoading
  | LoadingEvent

type ViewEventStateModal
  = InviteGuestsInfoModal
  | AttendeeSuccessModal

type EditEventState
  = EditEvent { picker: DP.DatePicker, input: EditEventInput }
  | LoadingEventToEdit

type alias PageState a = { key: Nav.Key
                         , timeZone : Time.Zone
                         , state: a
                         , pageUrl : Url
                         }

-- Msg
type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url
    | CurrentTimeIs Url Time.Zone Time.Posix
    | NewEventMsg NewEventMsg
    | ViewEventMsg ViewEventMsg
    | EditEventMsg EditEventMsg

type NewEventMsg
    = UpdateEventInput DP.DatePicker EventInput
    | UpdateEventStartTime (DP.DatePicker, Maybe Time.Posix)
    | OpenPicker
    | CreateEventMsg EventInput
    | CreatedEvent (Result Http.Error Event)

type ViewEventMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput
    | LoadedEvent (Result Http.Error Event)
    | CloseModal

type EditEventMsg
    = LoadedEventForEdit (Result Http.Error Event)

-- Event
type alias Event =
  { id             : String
  , title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Maybe Time.Posix
  , location       : String
  , attendees      : List Attendee
  -- , googleMapsLink : Maybe String
  }

type alias EventInput =
  { title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Maybe Time.Posix
  , location       : String
  -- , googleMapsLink : Maybe String
  , password       : String
  }

type alias EditEventInput =
  { id              : String
  , title          : String
  , description    : String
  , startTime      : Time.Posix
  , endTime        : Maybe Time.Posix
  , location       : String
  -- , googleMapsLink : Maybe String
  , password       : String
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
  , pageUrl = ps.pageUrl
  }

-- encoders and decoders
eventDecoder : D.Decoder Event
eventDecoder = D.map7 Event
                 (D.field "id" D.string)
                 (D.field "title" D.string)
                 (D.field "description" D.string)
                 (D.field "startTime" Iso8601.decoder)
                 (D.maybe (D.field "endTime" Iso8601.decoder))
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

emptyEventInput : Time.Posix -> EventInput
emptyEventInput startTime = { title = ""
                  , description = ""
                  , location = ""
                  , startTime = startTime
                  , endTime = Nothing
                  , password = ""
                  }

encodeEventInput : EventInput -> Value
encodeEventInput { title, description, location, startTime, endTime, password } = Encode.object
                                                      [ ("title", Encode.string title)
                                                      , ("description", Encode.string description)
                                                      , ("location", Encode.string location)
                                                      , ("startTime", Iso8601.encode startTime)
                                                      , ("endTime", Maybe.withDefault Encode.null <| Maybe.map Iso8601.encode endTime)
                                                      , ("password", Encode.string password)
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

