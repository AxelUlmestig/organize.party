module Page.NewEvent exposing (handleSubscription, update, view)

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
import Iso8601 as Iso8601
import Shared.SectionSeparator exposing (sectionSeparator)
import SingleDatePicker as DP
import Time as Time
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)
import Browser.Dom as Dom
import Task
import Process
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Platform.Sub as Sub
import Shared.EventEditor as EventEditor
import Dict exposing (Dict)

borderRadius =
    A.style "border-radius" "5px"


view : PageState NewEventState -> Html NewEventMsg
view pageState =
    case pageState.state of
        NewEventLoading ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
              ]

        NewEvent { picker, input } ->
            H.div []
              [ H.h1 [ A.style "margin-top" "1rem" , A.class "mb-3" ] [ H.text "Create an event" ]
              , H.map CreateEventEventEditorMsg <| EventEditor.view Dict.empty { timezone = pageState.timeZone, picker = picker, input = input }
              ]


update : NewEventMsg -> PageState NewEventState -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState
    in
    case msg of
        CreateEventEventEditorMsg (EventEditor.EventEditorInternalMsg internalMsg) ->
          case pageState.state of
            NewEvent eventEditorState ->
              let (newEventEditorState, newEventEditorMsg) = EventEditor.update internalMsg eventEditorState
              in ( format (NewEventState (NewEvent newEventEditorState)), Cmd.map (NewEventMsg << CreateEventEventEditorMsg) newEventEditorMsg )

            state -> ( format (NewEventState pageState.state), Cmd.none )

        CreateEventEventEditorMsg (EventEditor.EventEditorSubmit event) ->
            ( format (NewEventState NewEventLoading), createNewEvent event )

        CreatedEvent result ->
            case result of
                Ok event ->
                    ( format (ViewEventState (ViewEvent (Just InviteGuestsInfoModal) event (emptyAttendeeInput event.id))), Nav.pushUrl pageState.key ("/e/" ++ event.id) )

                Err _ ->
                    ( format Failure, Cmd.none )


createNewEvent : EventInput -> Cmd Msg
createNewEvent input =
    Http.post
        { url = "/api/v1/events"
        , expect = Http.expectJson (NewEventMsg << CreatedEvent) eventDecoder
        , body = Http.jsonBody (encodeEventInput input)
        }


handleSubscription : PageState NewEventState -> Sub Msg
handleSubscription pageState =
    case pageState.state of
        NewEvent eventState ->
            Sub.map (NewEventMsg << CreateEventEventEditorMsg) <| EventEditor.handleSubscription eventState
        _ ->
            Sub.none
