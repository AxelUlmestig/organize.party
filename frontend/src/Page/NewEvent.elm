module Page.NewEvent exposing (view, update)

import Browser
import Html as H exposing (Html)
import DurationDatePicker as DP
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import Time as Time
import Http
import Browser.Navigation as Nav

import Types exposing (..)

view : NewEventState -> Html Msg
view state =
  case state of
    NewEventLoading -> H.text "Loading..."
    NewEvent { picker, input } ->
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
            , H.button [ onClick (NewEventMsg (CreateEventMsg input)) ] [ H.text "Submit" ]
          ]

update : NewEventMsg -> PageState NewEventState -> ( PageState State, Cmd Msg )
update msg pageState =
  let
    format = \x -> mapPageState (always x) pageState
  in case msg of

    UpdateEventInput picker input ->
      let
        newPageState =
          { state = NewEventState (NewEvent { picker = picker, input = input })
          , timeZone = pageState.timeZone
          , key = pageState.key
          }
      in (newPageState, Cmd.none )

    UpdateEventStartTime (picker, mTime) ->
      case pageState.state of
        NewEvent oldState ->
          let
            (newStartTime, newEndTime) = Maybe.withDefault (oldState.input.startTime, oldState.input.endTime) mTime
            oldInput = oldState.input
            newEventState = NewEventState (NewEvent { picker = picker, input = { oldInput | startTime = newStartTime, endTime = newEndTime } })
            newPageState =
              { state = newEventState
              , timeZone = pageState.timeZone
              , key = pageState.key
              }
          in ( newPageState, Cmd.none )
        _ -> ( format (NewEventState pageState.state), Cmd.none )

    OpenPicker ->
      case pageState.state of
        NewEvent oldState ->
          let
            x = oldState
            newPicker = DP.openPicker (pickerSettings pageState.timeZone x.picker x.input) x.input.startTime (Just x.input.startTime) (Just x.input.endTime) x.picker
            newPageState =
              { state = NewEventState (NewEvent { x | picker = newPicker })
              , timeZone = pageState.timeZone
              , key = pageState.key
              }
          in ( newPageState, Cmd.none )
        _ -> ( format (NewEventState pageState.state), Cmd.none )


    CreateEventMsg input -> ( format (NewEventState NewEventLoading), createNewEvent input )

    CreatedEvent result ->
        case result of
            Ok event ->
                ( format (ViewEventState (ViewEvent event (emptyAttendeeInput event.id))), Nav.pushUrl pageState.key ("/e/" ++ event.id) )
            Err _ ->
                ( format Failure, Cmd.none )


pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
  let
    getValueFromPicker : ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
    getValueFromPicker (dp, mTime) = case mTime of
                                             Nothing -> NewEventMsg (UpdateEventInput dp input)
                                             Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput dp { input | startTime = newStart, endTime = newEnd })

  in DP.defaultSettings timeZone getValueFromPicker

createNewEvent : EventInput -> Cmd Msg
createNewEvent input = Http.post
                      { url = "/api/v1/events"
                      , expect = Http.expectJson (NewEventMsg << CreatedEvent) eventDecoder
                      , body = Http.jsonBody (encodeEventInput input)
                      }

