module Types exposing
    ( Attendee
    , AttendeeInput
    , AttendeeStatus(..)
    , Comment
    , EditEventInput
    , EditEventMsg(..)
    , EditEventState(..)
    , AboutState
    , AboutMsg
    , NewForgetMeRequestState(..)
    , NewForgetMeRequestMsg(..)
    , ForgetMeRequestState(..)
    , ForgetMeRequestMsg(..)
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
    , NavbarState(..)
    , NavbarMsg(..)
    , attendeeStatusToString
    , emptyAttendeeInput
    , emptyEventInput
    , encodeAttendeeInput
    , encodeAttendeeInputForRsvp
    , encodeAttendeeInputForComment
    , attendeeInputDecoder
    , encodeEditEventInput
    , encodeEventInput
    , eventDecoder
    , encodeNewForgetMeRequest
    , newForgetMeRequestResponseDecoder
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
    | AboutState AboutState
    | NewForgetMeRequestState NewForgetMeRequestState
    | ForgetMeRequestState ForgetMeRequestState


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



type EditEventState
    = LoadingEventToEdit
    | EditEvent Event (Maybe EditEventStateModal) EventEditor.EventEditorState
    | SubmittedEdit Event EventEditor.EventEditorState

type NavbarState
    = NavbarOpen
    | NavbarClosed


type EditEventStateModal
    = WrongPasswordModal

type alias AboutState = ()

type NewForgetMeRequestState
    = NewForgetMeRequestInputtingEmail String
    | NewForgetMeRequestLoading
    | NewForgetMeRequestSuccess String

type ForgetMeRequestState
    = ForgetMeRequestLoading
    | ForgetMeRequestConfirmation String
    | ForgetMeRequestSuccess Time.Posix

type alias PageState a =
    { key : Nav.Key
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    , state : a
    , pageUrl : Url
    , navbarState : NavbarState
    }



-- Msg


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url
    | CurrentTimeIs Url Time.Zone Time.Posix
    | NewEventMsg NewEventMsg
    | ViewEventMsg ViewEventMsg
    | EditEventMsg EditEventMsg
    | AboutMsg AboutMsg
    | NewForgetMeRequestMsg NewForgetMeRequestMsg
    | ForgetMeRequestMsg ForgetMeRequestMsg
    | NavbarMsg NavbarMsg
    | DoNothing


type NewEventMsg
    = CreateEventEventEditorMsg EventEditor.EventEditorMsg
    | CreatedEvent (Result Http.Error Event)


type ViewEventMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput
    | LoadedEvent (Result Http.Error Event)
    | CloseModal
    | RequestLocalStorageAttendeeInput String
    | CommentOnEvent AttendeeInput
    | CommentedOnEvent (Result Http.Error Event)


type EditEventMsg
    = LoadedEventForEdit (Result Http.Error Event)
    | EditedEvent (Result Http.Error Event)
    | CloseEditEventModal
    | EditEventEventEditorMsg EventEditor.EventEditorMsg

type alias AboutMsg
    = ()

type NewForgetMeRequestMsg
    = UpdateNewForgetMeRequestEmail String
    | SubmitNewForgetMetRequest String
    | SubmittedNewForgetMetRequest (Result Http.Error String)

type ForgetMeRequestMsg
    = SubmitForgetMeRequest String
    | SubmittedForgetMeRequest Time.Posix

type NavbarMsg
    = CloseNavbar
    | OpenNavbar

-- Event


type alias Event =
    { id : String
    , title : String
    , description : String
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , location : String
    , attendees : List Attendee
    , comments : List Comment
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
    , getNotifiedOnComments : Bool
    , comment : String
    , forceNotificationOnComment : Bool
    }


type AttendeeStatus
    = Coming
    | MaybeComing
    | NotComing

-- Comment

type alias Comment =
    { name : String
    , comment : String
    , timestamp : Time.Posix
    , gravatarUrl : String
    }


mapPageState : (a -> b) -> PageState a -> PageState b
mapPageState f ps =
    { state = f ps.state
    , timeZone = ps.timeZone
    , currentTime = ps.currentTime
    , key = ps.key
    , pageUrl = ps.pageUrl
    , navbarState = ps.navbarState
    }


setPageState : a -> PageState b -> PageState a
setPageState x =
    mapPageState (\_ -> x)



-- encoders and decoders


eventDecoder : D.Decoder Event
eventDecoder =
    D.map8 Event
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "startTime" Iso8601.decoder)
        (D.maybe (D.field "endTime" Iso8601.decoder))
        (D.field "location" D.string)
        (D.field "attendees" (D.list attendeeDecoder))
        (D.field "comments" (D.list commentDecoder))


attendeeDecoder : D.Decoder Attendee
attendeeDecoder =
    D.map4 Attendee
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.maybe <| D.field "comment" D.string)
        (D.field "plusOne" D.bool)

commentDecoder : D.Decoder Comment
commentDecoder =
    D.map4 Comment
        (D.field "commenterName" D.string)
        (D.field "comment" D.string)
        (D.field "timestamp" Iso8601.decoder)
        (D.field "gravatarUrl" D.string)

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
    , getNotifiedOnComments = False
    , comment = ""
    , forceNotificationOnComment = False
    }

encodeAttendeeInput : AttendeeInput -> Value
encodeAttendeeInput { eventId, email, name, status, plusOne, getNotifiedOnComments, comment, forceNotificationOnComment } =
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
        , ( "getNotifiedOnComments", Encode.bool getNotifiedOnComments )
        , ( "comment", Encode.string comment )
        , ( "forceNotificationOnComment", Encode.bool forceNotificationOnComment )
        ]


encodeAttendeeInputForRsvp : AttendeeInput -> Value
encodeAttendeeInputForRsvp { eventId, email, name, status, plusOne, getNotifiedOnComments } =
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
        , ( "getNotifiedOnComments", Encode.bool getNotifiedOnComments )
        ]

encodeAttendeeInputForComment : AttendeeInput -> Value
encodeAttendeeInputForComment { eventId, email, name, comment, forceNotificationOnComment } =
  Encode.object
    [ ( "eventId", Encode.string eventId )
    , ( "email", Encode.string (String.trim email) )
    , ( "name", Encode.string (String.trim name) )
    , ( "comment", Encode.string (String.trim comment) )
    , ( "forceNotificationOnComment", Encode.bool forceNotificationOnComment )
    ]

attendeeInputDecoder : D.Decoder AttendeeInput
attendeeInputDecoder =
    D.map8 AttendeeInput
        (D.field "eventId" D.string)
        (D.field "email" D.string)
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.field "plusOne" D.bool)
        (D.map (Maybe.withDefault False) <| D.maybe <| D.field "getNotifiedOnComments" D.bool)
        (D.field "comment" D.string)
        (D.map (Maybe.withDefault False) <| D.maybe <| D.field "forceNotificationOnComment" D.bool)

encodeNewForgetMeRequest : String -> Value
encodeNewForgetMeRequest email =
    Encode.object
        [ ( "email", Encode.string email ) ]

newForgetMeRequestResponseDecoder : D.Decoder String
newForgetMeRequestResponseDecoder = D.field "email" D.string
