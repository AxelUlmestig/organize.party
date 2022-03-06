module NewEvent exposing (view, update)

import Browser
import Html as H exposing (Html)
import DurationDatePicker as DP
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import Time as Time

import Types exposing (..)

view : { picker: DP.DatePicker, input: EventInput } -> Html Msg
view { picker, input } =
  let
    updatePicker : EventInput -> ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
    updatePicker input2 (picker2, mTimestamp) = case mTimestamp of
                                                  Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput picker2 { input2 | startTime = newStart, endTime = newEnd })
                                                  Nothing -> NewEventMsg (UpdateEventInput picker2 input2)
  in
    H.div [] [
        H.h3 [] [ H.text "Create A New Event" ]
        , H.div [] [ H.text "Title: ", H.input [ A.value input.title, onInput (\t -> NewEventMsg (UpdateEventInput picker { input | title = t })) ] [] ]
        , H.div [] [ H.text "Description: ", H.input [ A.value input.description, onInput (\d -> NewEventMsg (UpdateEventInput picker { input | description = d })) ] [] ]
        , H.div [] [ H.button [ onClick (NewEventMsg OpenPicker) ] [ H.text "click me" ], DP.view (DP.defaultSettings Time.utc (updatePicker input)) picker ]
        , H.div [] [ H.text "Location: ", H.input [ A.value input.location, onInput (\l -> NewEventMsg (UpdateEventInput picker { input | location = l })) ] [] ]
        , H.button [ onClick (CreateEventMsg input) ] [ H.text "Submit" ]
      ]


update : NewEventMsg -> PageState { picker: DP.DatePicker, input: EventInput } -> ( PageState State, Cmd Msg )
update msg pageState =
  case msg of
    UpdateEventInput picker input ->
      let
        newPageState =
          { state = NewEventState { picker = picker, input = input }
          , timeZone = pageState.timeZone
          , key = pageState.key
          }
      in (newPageState, Cmd.none )
    UpdateEventStartTime (picker, mTime) ->
      let
        (newStartTime, newEndTime) = Maybe.withDefault (pageState.state.input.startTime, pageState.state.input.endTime) mTime
        oldInput = pageState.state.input
        newEventState = NewEventState { picker = picker, input = { oldInput | startTime = newStartTime, endTime = newEndTime } }
        newPageState =
          { state = newEventState
          , timeZone = pageState.timeZone
          , key = pageState.key
          }
      in ( newPageState, Cmd.none )
    OpenPicker ->
      let
        x = pageState.state
        newPicker = DP.openPicker (pickerSettings pageState.timeZone x.picker x.input) x.input.startTime (Just x.input.startTime) (Just x.input.endTime) x.picker
        newPageState =
          { state = NewEventState { x | picker = newPicker }
          , timeZone = pageState.timeZone
          , key = pageState.key
          }
      in ( newPageState, Cmd.none )

pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
  let
    getValueFromPicker : ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
    getValueFromPicker (dp, mTime) = case mTime of
                                             Nothing -> NewEventMsg (UpdateEventInput dp input)
                                             Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput dp { input | startTime = newStart, endTime = newEnd })

  in DP.defaultSettings timeZone getValueFromPicker

