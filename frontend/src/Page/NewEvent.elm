module Page.NewEvent exposing (
    handleSubscription,
    update,
    view,
    init,
    State(..),
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
import Shared.PageState exposing (PageState, setPageState)

type State
    = NewEvent EventEditor.State
    | Loading
    | Failure

type Msg
    = InternalMsg InternalMsg
    | CreatedEvent Event

type InternalMsg
    = EventEditorMsg EventEditor.Msg
    | InternalCreatedEvent (Result Http.Error Event)


borderRadius =
    A.style "border-radius" "5px"

init : Time.Zone -> Time.Posix -> ( State, Cmd Msg )
init timezone time =
  ( NewEvent { timezone = timezone, picker = DP.init, input = emptyEventInput time }, Cmd.none )

view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        Loading ->
            H.div [ A.class "center" ]
              [ H.text "Creating event..."
              ]

        Failure ->
            H.div [ A.class "center" ]
              [ H.text "Something went wrong, please try again later"
              ]

        NewEvent { picker, input } ->
            H.div []
              [ H.h1 [ A.style "margin-top" "1rem" , A.class "mb-3" ] [ H.text "Create an event" ]
              , H.map (InternalMsg << EventEditorMsg) <| EventEditor.view Dict.empty { timezone = pageState.timeZone, picker = picker, input = input }
              ]


update : InternalMsg -> PageState navbarState State -> ( PageState navbarState State, Cmd Msg )
update msg pageState =
    case msg of
        EventEditorMsg (EventEditor.InternalMsg internalMsg) ->
          case pageState.state of
            NewEvent eventEditorState ->
              let (newEventEditorState, newEventEditorMsg) = EventEditor.update internalMsg eventEditorState
              in ( setPageState (NewEvent newEventEditorState) pageState, Cmd.map (InternalMsg << EventEditorMsg) newEventEditorMsg )

            state -> ( pageState, Cmd.none )

        EventEditorMsg (EventEditor.Submit event) ->
            let cmd =
                  Http.post
                      { url = "/api/v1/events"
                      , expect = Http.expectJson (InternalMsg << InternalCreatedEvent) eventDecoder
                      , body = Http.jsonBody (encodeEventInput event)
                      }
            in ( setPageState Loading pageState, cmd )

        InternalCreatedEvent result ->
            case result of
                Ok event ->
                    ( pageState, wrapCmd (CreatedEvent event) )

                Err _ ->
                    ( setPageState Failure pageState, Cmd.none )



handleSubscription : PageState navbarState State -> Sub Msg
handleSubscription pageState =
    case pageState.state of
        NewEvent eventState ->
            Sub.map (InternalMsg << EventEditorMsg) <| EventEditor.handleSubscription eventState
        _ ->
            Sub.none


wrapCmd : msg -> Cmd msg
wrapCmd msg = Task.perform (always msg) (Task.succeed ())
