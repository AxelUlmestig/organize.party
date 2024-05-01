module Types exposing
    ( Attendee
    , AttendeeInput
    , AttendeeStatus(..)
    , Comment
    , Event
    , EventInput
    , attendeeInputDecoder
    , attendeeStatusDecoder
    , attendeeStatusToString
    , emptyAttendeeInput
    , emptyEventInput
    , encodeAttendeeInput
    , encodeAttendeeInputForComment
    , encodeAttendeeInputForRsvp
    , encodeEventInput
    , encodeNewForgetMeRequest
    , eventDecoder
    , forgetMeRequestDecoder
    , newForgetMeRequestResponseDecoder
    )

import Browser
import Browser.Navigation as Nav
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import Maybe
import SingleDatePicker as DP
import Time
import Url exposing (Url)


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
    , gravatarUrl : Maybe String
    }



-- ForgetMeRequest


type alias ForgetMeRequest =
    { id : String
    , email : Maybe String
    , deletedAt : Maybe Time.Posix
    }



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
        (D.maybe (D.field "gravatarUrl" D.string))


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
newForgetMeRequestResponseDecoder =
    D.field "email" D.string


forgetMeRequestDecoder : D.Decoder ForgetMeRequest
forgetMeRequestDecoder =
    D.map3 ForgetMeRequest
        (D.field "id" D.string)
        (D.maybe <| D.field "email" D.string)
        (D.maybe <| D.field "deletedAt" Iso8601.decoder)
