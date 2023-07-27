module Shared.EventEditor exposing (EventEditorMsg(..), EventEditorState, view, update, handleSubscription)

import Time as Time
import SingleDatePicker as DP
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Shared.SectionSeparator exposing (sectionSeparator)
import Shared.ExpandingTextarea exposing (expandingTextarea)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Util exposing (viewEventDate, viewEventTime)
import Task
import Process
import Browser.Dom as Dom
import Dict exposing (Dict)

type alias EventEditorState =
  { picker : DP.DatePicker
  , input : EventInput
  , timezone : Time.Zone
  }

type alias EventInput =
  { title : String
  , description : String
  , startTime : Time.Posix
  , endTime : Maybe Time.Posix
  , location : String
  , password : String
  }

type EventEditorMsg
  = EventEditorSubmit EventInput
  | EventEditorInternalMsg EventEditorInternalMsg

type EventEditorInternalMsg
  = UpdateEventInput DP.DatePicker EventInput
  | UpdateEventStartTime ( DP.DatePicker, Maybe Time.Posix )
  | OpenPicker
  | FocusTimePicker
  | FocusTimePickerSoon
  | DoNothing

borderRadius =
    A.style "border-radius" "5px"

view : Dict String String -> EventEditorState -> Html EventEditorMsg
view copy { picker, input, timezone } =
  let
      updatePicker : EventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> EventEditorMsg
      updatePicker input2 ( picker2, mTimestamp ) =
          case mTimestamp of
              Just newStart ->
                  EventEditorInternalMsg (UpdateEventInput picker2 { input2 | startTime = newStart })

              Nothing ->
                  EventEditorInternalMsg (UpdateEventInput picker2 input2)
  in
  H.div []
      [ sectionSeparator "What"
      , H.div [] [ H.text "Event name" ]
      , H.div [] [ H.input [ A.class "padded-input", A.style "width" "100%", borderRadius, A.value input.title, onInput (\t -> EventEditorInternalMsg (UpdateEventInput picker { input | title = t })) ] [] ]
      , H.div [] [ H.text "Description" ]
      , expandingTextarea
          { text = input.description
          , onInput = (\d -> EventEditorInternalMsg (UpdateEventInput picker { input | description = d }))
          , placeholder = ""
          , styling = []
          }
      , sectionSeparator "When"
      , H.div [ A.style "display" "flex", A.style "color" "black", onClick (EventEditorInternalMsg OpenPicker) ]
          [ H.span [ A.style "flex" "2", A.class "d-flex flex-row justify-content-start" ]
              [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "100%", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                  [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.calendar) ]
              , H.input [ A.class "padded-input", A.readonly True, A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value (viewEventDate timezone input.startTime) ] []
              ]
          , H.span [ A.style "flex" "1", A.class "d-flex flex-row justify-content-start", A.style "margin-left" "0.5rem" ]
              [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "100%", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                  [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.clock) ]
              , H.input [ A.class "padded-input", A.readonly True, A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value (viewEventTime timezone input.startTime) ] []
              ]
          ]
      , DP.view (DP.defaultSettings timezone (updatePicker input)) picker
      , sectionSeparator "Where"
      , H.div [] [ H.text "Location" ]
      , H.div [ A.style "display" "flex", A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
          [ H.span [ A.style "flex" "2", A.class "d-flex flex-row justify-content-start" ]
            [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "100%", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.locationDot) ]
            , H.input [ A.class "padded-input", A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.location, onInput (\l -> EventEditorInternalMsg (UpdateEventInput picker { input | location = l })) ] []
            ]
          ]
      , sectionSeparator (Maybe.withDefault "Password For Future Edits" <| Dict.get "password_header" copy)
      -- , sectionSeparator "Password For Future Edits"
      , H.div [] [ H.text "Password" ]
      , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
          [ H.span [ A.style "flex" "2", A.class "d-flex flex-row justify-content-start" ]
            [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "100%", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.key) ]
            , H.input [ A.class "padded-input", A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.password, onInput (\pw -> EventEditorInternalMsg (UpdateEventInput picker { input | password = pw })) ] []
            ]
          ]
      , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
          [ H.button [ A.style "background-color" "#1c2c3b", onClick (EventEditorSubmit input), A.class "btn btn-primary" ] [ H.text "Submit" ]
          ]
      ]

update : EventEditorInternalMsg -> EventEditorState -> ( EventEditorState, Cmd EventEditorMsg )
update msg state =
  case msg of
    UpdateEventInput picker input ->
      ( { state | picker = picker, input = input }, Cmd.none )

    UpdateEventStartTime ( picker, mTime ) ->
      let
          newStartTime =
              Maybe.withDefault state.input.startTime mTime

          oldInput =
              state.input
      in ( { state | picker = picker, input = { oldInput | startTime = newStartTime } }, Cmd.none )

    OpenPicker ->
      let
          newPicker =
              DP.openPicker
                  (pickerSettings state.timezone state.picker state.input)
                  state.input.startTime
                  (Just state.input.startTime)
                  state.picker
      in ( { state | picker = newPicker }, focusTimePickerOrTryAgainLater )

    FocusTimePicker -> ( state, focusTimePickerOrTryAgainLater )
    FocusTimePickerSoon -> ( state, delay100ms (EventEditorInternalMsg FocusTimePicker) )
    DoNothing -> ( state, Cmd.none )


handleSubscription : EventEditorState -> Sub EventEditorMsg
handleSubscription { picker, input, timezone } =
  DP.subscriptions (pickerSettings timezone picker input) (EventEditorInternalMsg << UpdateEventStartTime) picker


focusTimePickerOrTryAgainLater : Cmd EventEditorMsg
focusTimePickerOrTryAgainLater =
  let
      handleFocusResult result =
        case result of
          Ok _ -> EventEditorInternalMsg DoNothing
          Err _ -> EventEditorInternalMsg FocusTimePickerSoon
  in Task.attempt handleFocusResult (Dom.focus "hour-select")

delay100ms : msg -> Cmd msg
delay100ms msg = Process.sleep 100 |> Task.perform (\_ -> msg)


pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings EventEditorMsg
pickerSettings timeZone picker input =
    let
        getValueFromPicker : ( DP.DatePicker, Maybe Time.Posix ) -> EventEditorMsg
        getValueFromPicker ( dp, mTime ) =
            case mTime of
                Nothing -> EventEditorInternalMsg (UpdateEventInput dp input)

                Just newStart -> EventEditorInternalMsg (UpdateEventInput dp { input | startTime = newStart })
    in
    DP.defaultSettings timeZone getValueFromPicker

