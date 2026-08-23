module Page.EditEvent exposing
    ( Msg(..)
    , State
    , handleSubscription
    , init
    , update
    , view
    )

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601
import Json.Encode as Encode exposing (Value)
import Platform.Sub as Sub
import Process
import Shared.EventEditor as EventEditor
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Shared.FormatUrls exposing (formatTextWithLinks)
import Shared.PageState exposing (PageState, mapPageState)
import Shared.SectionSeparator exposing (sectionSeparator)
import Shared.ViewAttendees exposing (viewAttendees)
import Shared.ViewComments exposing (viewComments)
import SingleDatePicker as DP
import Task
import Time
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)


type State
    = Loading
    | EditEvent Event (Maybe StateModal) EventEditor.State
    | PreparingEventInput Event EventEditor.State
    | SubmittedEdit Event EventEditor.State
    | Failure


type StateModal
    = WrongPasswordModal


type Msg
    = EditSuccessful Event
    | EditCancelled String
    | InternalMsg InternalMsg


type InternalMsg
    = LoadedEventForEdit (Result Http.Error Event)
    | EditResponse (Result Http.Error Event)
    | SubmitUpdate EventEditor.State
    | CloseModal
    | EventEditorMsg EventEditor.Msg


type alias EditEventInput =
    { id : String
    , title : String
    , description : String
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , location : String
    , password : String
    , photoId : Maybe String
    }


encodeEditEventInput : EditEventInput -> Value
encodeEditEventInput { title, description, location, startTime, endTime, password, photoId } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        , ( "location", Encode.string location )
        , ( "startTime", Iso8601.encode startTime )
        , ( "endTime", Maybe.withDefault Encode.null <| Maybe.map Iso8601.encode endTime )
        , ( "password", Encode.string password )
        , ( "photoId", Maybe.withDefault Encode.null <| Maybe.map Encode.string photoId )
        ]


copy : Dict String String
copy =
    Dict.insert "password_header" "Password" <| Dict.empty


view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        Loading ->
            H.div [ A.class "center" ]
                [ H.text "Loading..."
                ]

        PreparingEventInput _ eventEditorState ->
            H.div [ A.class "center" ]
                [ EventEditor.viewPhotoUploadStatus eventEditorState
                ]

        SubmittedEdit _ _ ->
            H.div [ A.class "center" ]
                [ H.text "Loading..."
                ]

        Failure ->
            H.div [ A.class "center" ]
                [ H.text "Something went wrong, please try again later"
                ]

        EditEvent event maybeModal eventEditorState ->
            H.div []
                [ case maybeModal of
                    Nothing ->
                        H.span [] []

                    Just modal ->
                        H.div [ A.class "modal-background" ]
                            [ H.div [ A.class "modal-window" ]
                                [ case modal of
                                    WrongPasswordModal ->
                                        H.div []
                                            [ H.text "Error: incorrect password"
                                            , H.div [ A.class "button-wrapper" ]
                                                [ H.button [ A.class "submit-button", onClick (InternalMsg CloseModal) ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.h1 [ A.class "page-title" ] [ H.text "Edit event" ]
                , H.map (InternalMsg << EventEditorMsg) (EventEditor.view copy eventEditorState)
                , H.div [ A.class "button-wrapper" ]
                    [ H.button [ A.class "submit-button secondary", onClick (EditCancelled event.id) ] [ H.text "Cancel" ]
                    , H.button [ A.class "submit-button", onClick (InternalMsg (SubmitUpdate eventEditorState)) ] [ H.text "Submit" ]
                    ]
                , viewAttendees event.attendees
                , H.h1 [ A.class "section-heading comments-header" ] [ H.text "Comments" ]
                , viewComments pageState.currentTime event.comments
                ]


update : InternalMsg -> PageState navbarState State -> ( PageState navbarState State, Cmd Msg )
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState
    in
    case msg of
        LoadedEventForEdit result ->
            case result of
                Ok event ->
                    let
                        editEventInput =
                            { title = event.title
                            , description = event.description
                            , startTime = event.startTime
                            , endTime = Nothing
                            , location = event.location
                            , password = ""
                            , photoId = Nothing
                            }

                        ( newState, newMsg ) =
                            let
                                ( eventEditorState, eventEditorMsg ) =
                                    EventEditor.init pageState.timeZone editEventInput event.photo
                            in
                            ( EditEvent event Nothing eventEditorState, Cmd.map (InternalMsg << EventEditorMsg) eventEditorMsg )
                    in
                    ( format newState, Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        EditResponse result ->
            case result of
                Ok event ->
                    ( pageState, wrapCmd (EditSuccessful event) )

                Err (Http.BadStatus 403) ->
                    case pageState.state of
                        SubmittedEdit event state ->
                            let
                                eventEditorState =
                                    { picker = state.picker
                                    , timezone = pageState.timeZone
                                    , photoUploader = state.photoUploader
                                    , input = state.input
                                    , maybeModal = Nothing
                                    }
                            in
                            ( format (EditEvent event (Just WrongPasswordModal) eventEditorState), Cmd.none )

                        _ ->
                            ( format Failure, Cmd.none )

                _ ->
                    ( format Failure, Cmd.none )

        CloseModal ->
            case pageState.state of
                EditEvent event _ input ->
                    ( format (EditEvent event Nothing input), Cmd.none )

                otherState ->
                    ( format otherState, Cmd.none )

        EventEditorMsg eventEditorMsg ->
            case eventEditorMsg of
                EventEditor.InternalMsg internalMsg ->
                    case pageState.state of
                        EditEvent event modal eventEditorState ->
                            let
                                ( newEventEditorState, newEventEditorMsg ) =
                                    EventEditor.update internalMsg eventEditorState
                            in
                            ( format (EditEvent event modal newEventEditorState), Cmd.map (InternalMsg << EventEditorMsg) newEventEditorMsg )

                        PreparingEventInput event eventEditorState ->
                            let
                                ( newEventEditorState, newEventEditorMsg ) =
                                    EventEditor.update internalMsg eventEditorState
                            in
                            ( format (PreparingEventInput event newEventEditorState), Cmd.map (InternalMsg << EventEditorMsg) newEventEditorMsg )

                        state ->
                            ( format pageState.state, Cmd.none )

                EventEditor.EventInputReady eventInput ->
                    case pageState.state of
                        PreparingEventInput event eventEditorState ->
                            let
                                editEventInput =
                                    { id = event.id
                                    , title = eventInput.title
                                    , description = eventInput.description
                                    , startTime = eventInput.startTime
                                    , endTime = eventInput.endTime
                                    , location = eventInput.location
                                    , password = eventInput.password
                                    , photoId = eventInput.photoId
                                    }
                            in
                            ( format (SubmittedEdit event eventEditorState), submitEdit editEventInput )

                        otherState ->
                            ( format otherState, Cmd.none )

        SubmitUpdate eventEditorState ->
            case pageState.state of
                EditEvent event _ _ ->
                    let
                        ( newEventEditorState, eventEditorCmd ) =
                            EventEditor.prepareInput eventEditorState
                    in
                    ( format (PreparingEventInput event eventEditorState), Cmd.map (InternalMsg << EventEditorMsg) eventEditorCmd )

                otherState ->
                    ( format otherState, Cmd.none )


init : String -> ( State, Cmd Msg )
init id =
    let
        cmd =
            Http.get
                { url = "/api/v1/events/" ++ id
                , expect = Http.expectJson (InternalMsg << LoadedEventForEdit) eventDecoder
                }
    in
    ( Loading, cmd )


submitEdit : EditEventInput -> Cmd Msg
submitEdit input =
    Http.request
        { url = "/api/v1/events/" ++ input.id ++ "/edit"
        , method = "PUT"
        , body = Http.jsonBody (encodeEditEventInput input)
        , headers = []
        , expect = Http.expectJson (InternalMsg << EditResponse) eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


handleSubscription : PageState navbarState State -> Sub Msg
handleSubscription pageState =
    case pageState.state of
        EditEvent _ _ eventState ->
            Sub.map (InternalMsg << EventEditorMsg) <| EventEditor.handleSubscription eventState

        _ ->
            Sub.none


wrapCmd : msg -> Cmd msg
wrapCmd msg =
    Task.perform (always msg) (Task.succeed ())
