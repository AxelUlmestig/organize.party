module ViewEvent exposing (view, update)

import Browser
import Html as H exposing (Html)
import DurationDatePicker as DP
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import Time as Time
import Basics.Extra exposing (uncurry)
import Date as Date
import Browser.Navigation as Nav
import Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as D
import Iso8601 as Iso8601

import Types exposing (..)

view : PageState ViewEventState -> Html Msg
view pageState =
  case pageState.state of
    AttendEventLoading -> H.text "Loading"
    ViewEvent event attendeeInput ->
      let
        { title, description, startTime, endTime, location, attendees } = event

        onStatusUpdate newStatus = ViewEventMsg (case newStatus of
                                    "Coming" -> UpdateAttendeeInput { attendeeInput | status = Coming }
                                    "Maybe Coming" -> UpdateAttendeeInput { attendeeInput | status = MaybeComing }
                                    "Not Coming" -> UpdateAttendeeInput { attendeeInput | status = NotComing }
                                    _ -> UpdateAttendeeInput attendeeInput)


        viewEventDate : Time.Posix -> Time.Posix -> Html Msg
        viewEventDate start end =
            let
                oneDayMillis = 24 * 60 * 60 * 1000
                timeDiff = Time.posixToMillis end - Time.posixToMillis start

                formatTime : Time.Posix -> String
                formatTime time = String.fromInt (Time.toHour pageState.timeZone time) ++ ":" ++ String.fromInt (Time.toMinute pageState.timeZone time)

                formatDate : Time.Posix -> String
                formatDate time = Date.toIsoString (Date.fromPosix pageState.timeZone time)
            in if timeDiff < oneDayMillis
            then H.div [] [ H.text (formatDate start ++ ", " ++ formatTime start ++ " - " ++ formatTime end) ]
            else H.div [] [ H.text (formatDate start ++ " " ++ formatTime start ++ ", " ++ formatDate end ++ " " ++ formatTime end) ]
      in
        H.div []
          [ H.div []
              [ H.div [] [ H.h1 [] [ H.text title ] ]
              , viewEventDate startTime endTime
              , H.div [] [ H.text location ]
              , H.div [] [ H.text description ]
              ]
          , H.br [] []
          , H.div []
              [ H.b [] [ H.text "Are you attending?" ]
              , H.div [] [ H.text "email: ", H.input [ A.value attendeeInput.email, onInput (\e -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | email = e })), A.placeholder "Your email" ] [] ]
              , H.div [] [ H.text "name: ", H.input [ A.value attendeeInput.name, onInput (\fn -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | name = fn })), A.placeholder "Your name" ] [] ]
              , H.div [] [ H.text "plus one? ", H.input [ A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | plusOne = po })) ] [] ]
              , H.div []
                  [ H.select [ onInput onStatusUpdate ]
                      [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                      , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                      , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                      ]
                  ]
              , H.button [ onClick (ViewEventMsg (AttendMsg attendeeInput)) ] [ H.text "Submit" ]
              ]
          , H.br [] []
          , H.h3 [] [ H.text "Attendees" ]
          , H.table []
             ( H.tr [] [ H.th [] [ H.text "Name" ], H.th [] [ H.text "Coming?" ], H.th [] [ H.text "Plus One?" ] ]
             :: (List.map (\{name, status, plusOne} -> H.tr [] [ H.td [] [ H.text name ]
                                                                              , H.td [] [ H.text (attendeeStatusToString status) ]
                                                                              , H.td [] [ H.text (if plusOne then "Yes" else "No") ]
                                                                              ]) attendees)
             )
          ]


update : ViewEventMsg -> PageState ViewEventState -> ( PageState State, Cmd Msg )
update msg pageState =
  let
    format = \x -> mapPageState (always x) pageState
    { key, timeZone, state } = pageState
  in case msg of

    AttendedEvent result ->
      case result of
        Ok attendedEvent ->
          ( format (ViewEventState (ViewEvent attendedEvent (emptyAttendeeInput attendedEvent.id))), Nav.pushUrl key ("/e/" ++ attendedEvent.id) )
        Err _ ->
          ( format Failure, Cmd.none )

    UpdateAttendeeInput input ->
      case pageState.state of
        ViewEvent event _ -> ( format (ViewEventState (ViewEvent event input)), Cmd.none )
        _ -> ( format (ViewEventState pageState.state), Cmd.none )

    AttendMsg input -> ( format (ViewEventState AttendEventLoading), attendEvent input )


attendEvent : AttendeeInput -> Cmd Msg
attendEvent input = Http.request
                      { method = "PUT"
                      , headers = []
                      , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
                      , expect = Http.expectJson (ViewEventMsg << AttendedEvent) eventDecoder
                      , body = Http.jsonBody (encodeAttendeeInput input)
                      , timeout = Nothing
                      , tracker = Nothing
                      }

