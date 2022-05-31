module Page.NewEvent exposing (view, update)

import Browser
import Html as H exposing (Html)
import DurationDatePicker as DP
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import Time as Time
import Http
import Browser.Navigation as Nav
import Iso8601 as Iso8601

import Types exposing (..)
import Util exposing (viewEventDate)

borderRadius = A.style "border-radius" "5px"

view : PageState NewEventState -> Html Msg
view pageState =
  case pageState.state of
    NewEventLoading -> H.text "Loading..."
    NewEvent { picker, input } ->
      let
        updatePicker : EventInput -> ( DP.DatePicker, Maybe (Time.Posix, Time.Posix) ) -> Msg
        updatePicker input2 (picker2, mTimestamp) = case mTimestamp of
                                                      Just (newStart, newEnd) -> NewEventMsg (UpdateEventInput picker2 { input2 | startTime = newStart, endTime = newEnd })
                                                      Nothing -> NewEventMsg (UpdateEventInput picker2 input2)

        eventTimeString = viewEventDate pageState.timeZone input.startTime input.endTime
      in
        H.div []
            [ H.h1 [ A.class "mb-3" ] [ H.text "Create an event" ]
            , H.div [ A.class "d-flex flex-row justify-content-start" ]
              [ H.h5 [ A.class "mb-4" ] [ H.text "What" ]
              , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
              ]

            , H.div [] [ H.text "Event name" ]
            , H.div [] [ H.input [ A.style "width" "100%", borderRadius, A.value input.title, onInput (\t -> NewEventMsg (UpdateEventInput picker { input | title = t })) ] [] ]

            , H.div [] [ H.text "Description" ]
            , H.div [] [ H.textarea [ A.style "width" "100%", borderRadius, A.value input.description, onInput (\d -> NewEventMsg (UpdateEventInput picker { input | description = d })) ] [] ]

            , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
              [ H.h5 [ A.class "mb-4" ] [ H.text "When" ]
              , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
              ]

            , H.div [] [
                H.button [ onClick (NewEventMsg OpenPicker) ] [eventTimeString]
                , DP.view (DP.defaultSettings pageState.timeZone (updatePicker input)) picker
                ]

            , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
              [ H.h5 [ A.class "mb-4" ] [ H.text "Where" ]
              , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
              ]


            -- This icon is not showing up for some reason...
            , H.node "svg" [ A.class "bi", A.width 32, A.height 32, A.attribute "fill" "currentColor" ] [ H.node "use" [ A.attribute "xlink:href" "bootstrap-icons.svg#heart-fill" ] [] ]

            , H.div [] [ H.text "Location" ]
            , H.div [] [ H.input [ A.style "width" "100%", borderRadius, A.value input.location, onInput (\l -> NewEventMsg (UpdateEventInput picker { input | location = l })) ] [] ]

            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ] [
                H.button [ A.style "background-color" "#1c2c3b", onClick (NewEventMsg (CreateEventMsg input)), A.class "btn btn-primary" ] [ H.text "Submit" ]
                ]
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
            newPicker = DP.openPicker
                  (pickerSettings pageState.timeZone oldState.picker oldState.input)
                  oldState.input.startTime
                  (Just oldState.input.startTime)
                  (Just oldState.input.endTime)
                  oldState.picker

            newPageState =
              { state = NewEventState (NewEvent { oldState | picker = newPicker })
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
