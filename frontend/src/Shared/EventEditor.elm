module Shared.EventEditor exposing (Msg(..), State, view, update, handleSubscription)

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
import Types exposing (EventInput)

type alias State =
  { picker : DP.DatePicker
  , input : EventInput
  , timezone : Time.Zone
  }

type Msg
  = Submit EventInput
  | InternalMsg InternalMsg

type InternalMsg
  = UpdateEventInput DP.DatePicker EventInput
  | UpdateEventStartTime ( DP.DatePicker, Maybe Time.Posix )
  | OpenPicker
  | FocusTimePicker
  | FocusTimePickerSoon
  | DoNothing

borderRadius =
    A.style "border-radius" "5px"

view : Dict String String -> State -> Html Msg
view copy { picker, input, timezone } =
  let
      updatePicker : EventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> Msg
      updatePicker input2 ( picker2, mTimestamp ) =
          case mTimestamp of
              Just newStart ->
                  InternalMsg (UpdateEventInput picker2 { input2 | startTime = newStart })

              Nothing ->
                  InternalMsg (UpdateEventInput picker2 input2)
  in
  H.div []
      [ sectionSeparator "What"
      , H.div [] [ H.text "Event name" ]
      , H.div [] [ H.input [ A.attribute "data-testid" "event-editor-event-name", A.class "padded-input", A.style "width" "100%", borderRadius, A.value input.title, onInput (\t -> InternalMsg (UpdateEventInput picker { input | title = t })) ] [] ]
      , H.div [] [ H.text "Description" ]
      , expandingTextarea
          { text = input.description
          , onInput = (\d -> InternalMsg (UpdateEventInput picker { input | description = d }))
          , placeholder = ""
          , styling = []
          }
      , sectionSeparator "When"
      , H.div [ A.style "display" "flex", A.style "color" "black", onClick (InternalMsg OpenPicker) ]
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
            , H.input [ A.attribute "data-testid" "event-editor-event-location", A.class "padded-input", A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.location, onInput (\l -> InternalMsg (UpdateEventInput picker { input | location = l })) ] []
            ]
          ]
      , sectionSeparator (Maybe.withDefault "Password For Future Edits" <| Dict.get "password_header" copy)
      -- , sectionSeparator "Password For Future Edits"
      , H.div [] [ H.text "Password" ]
      , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
          [ H.span [ A.style "flex" "2", A.class "d-flex flex-row justify-content-start" ]
            [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "100%", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.key) ]
            , H.input [ A.attribute "data-testid" "event-editor-event-password",  A.class "padded-input", A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.password, onInput (\pw -> InternalMsg (UpdateEventInput picker { input | password = pw })) ] []
            ]
          ]
      , H.div [ A.class "text-center", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
          [ H.button [ A.attribute "data-testid" "event-editor-event-submit-button", A.style "background-color" "#1c2c3b", onClick (Submit input), A.class "btn btn-primary" ] [ H.text "Submit" ]
          ]
      ]

update : InternalMsg -> State -> ( State, Cmd Msg )
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
    FocusTimePickerSoon -> ( state, delay100ms (InternalMsg FocusTimePicker) )
    DoNothing -> ( state, Cmd.none )


handleSubscription : State -> Sub Msg
handleSubscription { picker, input, timezone } =
  DP.subscriptions (pickerSettings timezone picker input) (InternalMsg << UpdateEventStartTime) picker


focusTimePickerOrTryAgainLater : Cmd Msg
focusTimePickerOrTryAgainLater =
  let
      handleFocusResult result =
        case result of
          Ok _ -> InternalMsg DoNothing
          Err _ -> InternalMsg FocusTimePickerSoon
  in Task.attempt handleFocusResult (Dom.focus "hour-select")

delay100ms : msg -> Cmd msg
delay100ms msg = Process.sleep 100 |> Task.perform (\_ -> msg)


pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
    let
        getValueFromPicker : ( DP.DatePicker, Maybe Time.Posix ) -> Msg
        getValueFromPicker ( dp, mTime ) =
            case mTime of
                Nothing -> InternalMsg (UpdateEventInput dp input)

                Just newStart -> InternalMsg (UpdateEventInput dp { input | startTime = newStart })
    in
    DP.defaultSettings timeZone getValueFromPicker

