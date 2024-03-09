module Page.EditEvent exposing (fetchEvent, handleSubscription, update, view)

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

copy : Dict String String
copy = Dict.insert "password_header" "Password" <| Dict.empty

view : PageState EditEventState -> Html EditEventMsg
view pageState =
    case pageState.state of
        LoadingEventToEdit ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
              ]

        SubmittedEdit _ _ ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
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
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick CloseEditEventModal, A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.h1 [ A.class "mb-3" ] [ H.text "Edit event" ]
                , H.map EditEventEventEditorMsg (EventEditor.view copy { timezone = pageState.timeZone, picker = picker, input = input })
                , viewAttendees event.attendees
                , H.h1 [ A.class "mb-3" ] [ H.text "Comments" ]
                , viewComments pageState.currentTime event.comments
                ]


update : EditEventMsg -> PageState EditEventState -> ( PageState State, Cmd Msg )
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

                        newState = EditEventState (EditEvent event Nothing { timezone = pageState.timeZone, picker = DP.init, input = editEventInput })
                    in
                    ( format newState, Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        EditedEvent result ->
            case result of
                Ok event ->
                    let
                        newState =
                            format (ViewEventState (ViewEvent Nothing event (emptyAttendeeInput event.id)))

                        cmd =
                            Cmd.batch
                              [ Nav.pushUrl pageState.key ("/e/" ++ event.id)
                              , Task.perform ViewEventMsg <| Task.succeed <| RequestLocalStorageAttendeeInput event.id
                              ]
                    in
                    ( newState, cmd )

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
                            in ( format (EditEventState (EditEvent event (Just WrongPasswordModal) eventEditorState)), Cmd.none )

                        _ ->
                            ( format Failure, Cmd.none )

                _ ->
                    ( format Failure, Cmd.none )

        CloseEditEventModal ->
            case pageState.state of
                EditEvent event _ input ->
                    ( format (EditEventState (EditEvent event Nothing input)), Cmd.none )

                otherState ->
                    ( format (EditEventState otherState), Cmd.none )

        EditEventEventEditorMsg (EventEditor.EventEditorInternalMsg internalMsg) ->
          case pageState.state of
            EditEvent event modal eventEditorState ->
              let (newEventEditorState, newEventEditorMsg) = EventEditor.update internalMsg eventEditorState
              in ( format (EditEventState (EditEvent event modal newEventEditorState)), Cmd.map (EditEventMsg << EditEventEventEditorMsg) newEventEditorMsg )

            state -> ( format (EditEventState pageState.state), Cmd.none )

        EditEventEventEditorMsg (EventEditor.EventEditorSubmit _) ->
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
              in ( format (EditEventState (SubmittedEdit event state)), submitEdit editEventInput)
            otherState ->
              ( format (EditEventState otherState), Cmd.none )

fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (EditEventMsg << LoadedEventForEdit) eventDecoder
        }


submitEdit : EditEventInput -> Cmd Msg
submitEdit input =
    Http.request
        { url = "/api/v1/events/" ++ input.id ++ "/edit"
        , method = "PUT"
        , body = Http.jsonBody (encodeEditEventInput input)
        , headers = []
        , expect = Http.expectJson (EditEventMsg << EditedEvent) eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


handleSubscription : PageState EditEventState -> Sub Msg
handleSubscription pageState =
    case pageState.state of
        EditEvent _ _ eventState ->
            Sub.map (EditEventMsg << EditEventEventEditorMsg) <| EventEditor.handleSubscription eventState
        _ ->
            Sub.none
