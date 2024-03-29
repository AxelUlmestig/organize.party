module Page.EditEvent exposing (
    init,
    handleSubscription,
    update,
    view,
    State,
    Msg(..)
  )

import Browser
import Browser.Navigation as Nav
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
import Shared.SectionSeparator exposing (sectionSeparator)
import Shared.ViewAttendees exposing (viewAttendees)
import SingleDatePicker as DP
import Time as Time
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)
import Browser.Dom as Dom
import Task
import Process
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Shared.FormatUrls exposing (formatTextWithLinks)
import Platform.Sub as Sub
import Shared.EventEditor as EventEditor
import Dict exposing (Dict)
import Task
import Shared.ViewComments exposing (viewComments)
import Json.Encode as Encode exposing (Value)
import Iso8601 as Iso8601

type State
    = Loading
    | EditEvent Event (Maybe StateModal) EventEditor.State
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
    | CloseEditEventModal
    | EventEditorMsg EventEditor.Msg

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

copy : Dict String String
copy = Dict.insert "password_header" "Password" <| Dict.empty

view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        Loading ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
              ]

        SubmittedEdit _ _ ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
              ]

        Failure ->
            H.div [ A.class "center" ]
              [ H.text "Something went wrong, please try again later"
              ]

        EditEvent event maybeModal { picker, input } ->
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
                                            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick (InternalMsg CloseEditEventModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.h1 [ A.class "mb-3" ] [ H.text "Edit event" ]
                , H.map (InternalMsg << EventEditorMsg) (EventEditor.view copy { timezone = pageState.timeZone, picker = picker, input = input })
                , viewAttendees event.attendees
                , H.h1 [ A.class "mb-3" ] [ H.text "Comments" ]
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
                            }

                        newState = EditEvent event Nothing { timezone = pageState.timeZone, picker = DP.init, input = editEventInput }
                    in
                    ( format newState, Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        EditResponse result ->
            case result of
                Ok event -> ( pageState, wrapCmd (EditSuccessful event) )

                Err (Http.BadStatus 403) ->
                    case pageState.state of
                        SubmittedEdit event state ->
                            let eventEditorState =
                                  { picker = state.picker
                                  , timezone = pageState.timeZone
                                  , input =
                                    { title = state.input.title
                                    , description = state.input.description
                                    , startTime = state.input.startTime
                                    , endTime = state.input.endTime
                                    , location = state.input.location
                                    , password = state.input.password
                                    }
                                  }
                            in ( format (EditEvent event (Just WrongPasswordModal) eventEditorState), Cmd.none )

                        _ ->
                            ( format Failure, Cmd.none )

                _ ->
                    ( format Failure, Cmd.none )

        CloseEditEventModal ->
            case pageState.state of
                EditEvent event _ input ->
                    ( format (EditEvent event Nothing input), Cmd.none )

                otherState ->
                    ( format otherState, Cmd.none )

        EventEditorMsg (EventEditor.InternalMsg internalMsg) ->
          case pageState.state of
            EditEvent event modal eventEditorState ->
              let (newEventEditorState, newEventEditorMsg) = EventEditor.update internalMsg eventEditorState
              in ( format (EditEvent event modal newEventEditorState), Cmd.map (InternalMsg << EventEditorMsg) newEventEditorMsg )

            state -> ( format pageState.state, Cmd.none )

        EventEditorMsg (EventEditor.Submit _) ->
          case pageState.state of
            EditEvent event _ state ->
              let editEventInput =
                    { id = event.id
                    , title = state.input.title
                    , description = state.input.description
                    , startTime = state.input.startTime
                    , endTime = state.input.endTime
                    , location = state.input.location
                    , password = state.input.password
                    }
              in ( format (SubmittedEdit event state), submitEdit editEventInput)
            otherState ->
              ( format otherState, Cmd.none )

init : String -> ( State, Cmd Msg )
init id =
    let cmd =
          Http.get
            { url = "/api/v1/events/" ++ id
            , expect = Http.expectJson (InternalMsg << LoadedEventForEdit) eventDecoder
            }
    in ( Loading, cmd )


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
wrapCmd msg = Task.perform (always msg) (Task.succeed ())
