module Types exposing
    ( Attendee
    , AttendeeInput
    , AttendeeStatus(..)
    , EditEventInput
    , EditEventMsg(..)
    , EditEventState(..)
    , EditEventStateModal(..)
    , Event
    , EventInput
    , Msg(..)
    , NewEventMsg(..)
    , NewEventState(..)
    , PageState
    , State(..)
    , ViewEventMsg(..)
    , ViewEventState(..)
    , ViewEventStateModal(..)
    , attendeeStatusToString
    , emptyAttendeeInput
    , emptyEventInput
    , encodeAttendeeInput
    , encodeEditEventInput
    , encodeEventInput
    , eventDecoder
    , mapPageState
    , setPageState
    )

import Browser
import Browser.Navigation as Nav
import Http
import Iso8601 as Iso8601
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import SingleDatePicker as DP
import Time as Time
import Url exposing (Url)
import TimePicker as TP


-- State


type State
    = Loading
    | Failure
    | NewEventState NewEventState
    | ViewEventState ViewEventState
    | EditEventState EditEventState


type NewEventState
    = NewEvent { datePicker : DP.DatePicker, timePicker : TP.TimePicker, input : EventInput }
    | NewEventLoading


type ViewEventState
    = ViewEvent (Maybe ViewEventStateModal) Event AttendeeInput
    | AttendEventLoading
    | LoadingEvent
    | EventNotFound


type ViewEventStateModal
    = InviteGuestsInfoModal
    | AttendeeSuccessModal


type EditEventState
    = LoadingEventToEdit
    | EditEvent (List Attendee) (Maybe EditEventStateModal) { picker : DP.DatePicker, input : EditEventInput }
    | SubmittedEdit (List Attendee) { picker : DP.DatePicker, input : EditEventInput }


type EditEventStateModal
    = WrongPasswordModal


type alias PageState a =
    { key : Nav.Key
    , timeZone : Time.Zone
    , state : a
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
    = UpdateEventInput DP.DatePicker TP.TimePicker EventInput
    | UpdateEventStartDate ( DP.DatePicker, Maybe Time.Posix )
    | OpenPicker
    | CreateEventMsg EventInput
    | CreatedEvent (Result Http.Error Event)
    | UpdateEventStartTime TP.TimePicker Time.Posix


type ViewEventMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput
    | LoadedEvent (Result Http.Error Event)
    | CloseModal


type EditEventMsg
    = LoadedEventForEdit (Result Http.Error Event)
    | UpdateEditEventInput DP.DatePicker EditEventInput
    | EditEventOpenPicker
    | SubmitEdit
    | EditedEvent (Result Http.Error Event)
    | CloseEditEventModal



-- Event


type alias Event =
    { id : String
    , title : String
    , description : String
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , location : String
    , attendees : List Attendee

    -- , googleMapsLink : Maybe String
    }


type alias EventInput =
    { title : String
    , description : String
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , location : String

    -- , googleMapsLink : Maybe String
    , password : String
    }


type alias EditEventInput =
    { id : String
    , title : String
    , description : String
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , location : String

    -- , googleMapsLink : Maybe String
    , password : String
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


setPageState : a -> PageState b -> PageState a
setPageState x =
    mapPageState (\_ -> x)



-- encoders and decoders


eventDecoder : D.Decoder Event
eventDecoder =
    D.map7 Event
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "startTime" Iso8601.decoder)
        (D.maybe (D.field "endTime" Iso8601.decoder))
        (D.field "location" D.string)
        (D.field "attendees" (D.list attendeeDecoder))


attendeeDecoder : D.Decoder Attendee
attendeeDecoder =
    D.map3 Attendee
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.field "plusOne" D.bool)


attendeeStatusDecoder : D.Decoder AttendeeStatus
attendeeStatusDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Coming" ->
                        D.succeed Coming

                    "MaybeComing" ->
                        D.succeed MaybeComing

                    "NotComing" ->
                        D.succeed NotComing

                    somethingElse ->
                        D.fail ("Unknown status: " ++ somethingElse)
            )


attendeeStatusToString : AttendeeStatus -> String
attendeeStatusToString status =
    case status of
        Coming ->
            "Coming"

        MaybeComing ->
            "Maybe Coming"

        NotComing ->
            "Not Coming"


emptyEventInput : Time.Posix -> EventInput
emptyEventInput startTime =
    { title = ""
    , description = ""
    , location = ""
    , startTime = startTime
    , endTime = Nothing
    , password = ""
    }


encodeEventInput : EventInput -> Value
encodeEventInput { title, description, location, startTime, endTime, password } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        , ( "location", Encode.string location )
        , ( "startTime", Iso8601.encode startTime )
        , ( "endTime", Maybe.withDefault Encode.null <| Maybe.map Iso8601.encode endTime )
        , ( "password", Encode.string password )
        ]


encodeEditEventInput : EditEventInput -> Value
encodeEditEventInput { title, description, location, startTime, endTime, password } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        , ( "location", Encode.string location )
        , ( "startTime", Iso8601.encode startTime )
        , ( "endTime", Maybe.withDefault Encode.null <| Maybe.map Iso8601.encode endTime )
        , ( "password", Encode.string password )
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
        encodeAttendeeStatus ai =
            case ai of
                Coming ->
                    Encode.string "Coming"

                MaybeComing ->
                    Encode.string "MaybeComing"

                NotComing ->
                    Encode.string "NotComing"
    in
    Encode.object
        [ ( "eventId", Encode.string eventId )
        , ( "email", Encode.string (String.trim email) )
        , ( "name", Encode.string (String.trim name) )
        , ( "status", encodeAttendeeStatus status )
        , ( "plusOne", Encode.bool plusOne )
        ]
