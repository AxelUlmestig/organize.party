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
    , DisplayComment
    , attendeeStatusToString
    , emptyAttendeeInput
    , emptyEventInput
    , encodeAttendeeInput
    , attendeeInputDecoder
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
import Shared.EventEditor as EventEditor
import Maybe



-- State


type State
    = Loading
    | Failure
    | NewEventState NewEventState
    | ViewEventState ViewEventState
    | EditEventState EditEventState


type NewEventState
    = NewEvent EventEditor.EventEditorState
    | NewEventLoading


type ViewEventState
    = ViewEvent (Maybe ViewEventStateModal) Event AttendeeInput
    | AttendEventLoading
    | LoadingEvent
    | EventNotFound


type ViewEventStateModal
    = InviteGuestsInfoModal
    | AttendeeSuccessModal
    | ViewEventAttendeeCommentModal String String


type EditEventState
    = LoadingEventToEdit
    | EditEvent String (List Attendee) (Maybe EditEventStateModal) EventEditor.EventEditorState
    | SubmittedEdit String (List Attendee) EventEditor.EventEditorState


type EditEventStateModal
    = WrongPasswordModal
    | EditEventAttendeeCommentModal DisplayComment


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
    | DoNothing


type NewEventMsg
    = CreateEventEventEditorMsg EventEditor.EventEditorMsg
    | CreatedEvent (Result Http.Error Event)


type ViewEventMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput
    | LoadedEvent (Result Http.Error Event)
    | ViewEventDisplayComment DisplayComment
    | CloseModal
    | RequestLocalStorageAttendeeInput String


type EditEventMsg
    = LoadedEventForEdit (Result Http.Error Event)
    | EditedEvent (Result Http.Error Event)
    | CloseEditEventModal
    | EditEventEventEditorMsg EventEditor.EventEditorMsg
    | EditEventDisplayComment DisplayComment

-- View comment

type alias DisplayComment =
  { name : String
  , comment : String
  }

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
    , comment : Maybe String
    , plusOne : Bool
    }


type alias AttendeeInput =
    { eventId : String
    , email : String
    , name : String
    , status : AttendeeStatus
    , plusOne : Bool
    , comment : String
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
    D.map4 Attendee
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.maybe <| D.field "comment" D.string)
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
    , comment = ""
    , status = Coming
    , plusOne = False
    }


encodeAttendeeInput : AttendeeInput -> Value
encodeAttendeeInput { eventId, email, name, comment, status, plusOne } =
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
        , ( "comment", if comment == "" then Encode.null else Encode.string comment )
        ]

attendeeInputDecoder : D.Decoder AttendeeInput
attendeeInputDecoder =
    D.map6 AttendeeInput
        (D.field "eventId" D.string)
        (D.field "email" D.string)
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.field "plusOne" D.bool)
        (D.map (Maybe.withDefault "") <| D.maybe <| D.field "comment" D.string)

