module Page.EditEvent exposing (view, update, fetchEvent)

import Browser
import Html as H exposing (Html)
import Http
import Browser.Navigation as Nav
import SingleDatePicker as DP
import Time as Time
import Html.Attributes as A
import Html.Events exposing (on, onInput, onClick, onCheck)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon

import Util exposing (viewEventDate, viewEventTime)
import Types exposing (..)

borderRadius = A.style "border-radius" "5px"

view : PageState EditEventState -> Html Msg
view pageState =
  case pageState.state of
    LoadingEventToEdit -> H.text "Loading..."
    SubmittedEdit _ -> H.text "Loading"
    EditEvent maybeModal { picker, input } ->
      let
        updatePicker : EditEventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> Msg
        updatePicker input2 (picker2, mTimestamp) = case mTimestamp of
                                                      Just newStart -> EditEventMsg (UpdateEditEventInput picker2 { input2 | startTime = newStart })
                                                      Nothing -> EditEventMsg (UpdateEditEventInput picker2 input2)
      in
        H.div []
          [ case maybeModal of
              Nothing -> H.span [] []
              Just modal ->
                H.div [ A.class "modal" ]
                  [ H.div [ A.class "modal_window" ]
                    [ case modal of
                        WrongPasswordModal ->
                          H.div []
                          [ H.text "Error: incorrect password"
                          , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                            [ H.button [ A.style "background-color" "#1c2c3b", onClick (EditEventMsg CloseEditEventModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                            ]
                          ]
                    ]
                  ]

          , H.h1 [ A.class "mb-3" ] [ H.text "Edit event" ]
          , H.div [ A.class "d-flex flex-row justify-content-start" ]
            [ H.h5 [ A.class "mb-4" ] [ H.text "What" ]
            , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
            ]

          , H.div [] [ H.text "Event name" ]
          , H.div [] [ H.input [ A.style "width" "100%", borderRadius, A.value input.title, onInput (\t -> EditEventMsg (UpdateEditEventInput picker { input | title = t })) ] [] ]

          , H.div [] [ H.text "Description" ]
          , H.div [] [ H.textarea [ A.style "width" "100%", borderRadius, A.value input.description, onInput (\d -> EditEventMsg (UpdateEditEventInput picker { input | description = d })) ] [] ]

          , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
            [ H.h5 [ A.class "mb-4" ] [ H.text "When" ]
            , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
            ]

          , H.div [ A.style "display" "flex", A.style "color" "black", onClick (EditEventMsg EditEventOpenPicker) ]
            [ H.span [ A.style "flex" "2", A.class "d-flex flex-row justify-content-start" ]
              [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.calendar) ]
                , H.input [ A.readonly True, A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value (viewEventDate pageState.timeZone input.startTime) ] []
              ]
            , H.span [ A.style "flex" "1", A.class "d-flex flex-row justify-content-start", A.style "margin-left" "0.5rem" ]
              [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.clock) ]
                , H.input [ A.readonly True, A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value (viewEventTime pageState.timeZone input.startTime) ] []
              ]
            ]
          , DP.view (DP.defaultSettings pageState.timeZone (updatePicker input)) picker

          , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
            [ H.h5 [ A.class "mb-4" ] [ H.text "Where" ]
            , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
            ]

          , H.div [] [ H.text "Location" ]
          , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
            [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
              [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.locationDot) ]
            , H.input [ A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.location, onInput (\l -> EditEventMsg (UpdateEditEventInput picker { input | location = l })) ] []
            ]

          , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
            [ H.h5 [ A.class "mb-4", A.style "white-space" "nowrap" ] [ H.text "Password" ]
            , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem" ] []
            ]

          , H.div [] [ H.text "Password" ]
          , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
            [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
              [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.key) ]
            , H.input [ A.style "width" "100%", A.style "border-radius" "0 5px 5px 0", A.value input.password, onInput (\pw -> EditEventMsg (UpdateEditEventInput picker { input | password = pw })) ] []
            ]

          , H.div [ A.class "text-center", A.style "margin-top" "1rem" ] [
              H.button [ A.style "background-color" "#1c2c3b", onClick (EditEventMsg (SubmitEdit { picker = picker, input = input })), A.class "btn btn-primary" ] [ H.text "Submit" ]
              ]
          ]

update : EditEventMsg -> PageState EditEventState -> ( PageState State, Cmd Msg )
update msg pageState =
  let
    format = \x -> mapPageState (always x) pageState
  in case msg of
    LoadedEventForEdit result ->
      case result of
        Ok event ->
          let
            editEventInput =
              { id = event.id
              , title = event.title
              , description = event.description
              , startTime = event.startTime
              , endTime = Nothing
              , location = event.location
              , password = ""
              }
            newState = EditEventState (EditEvent Nothing { picker = DP.init, input = editEventInput })
            newMsg = Nav.pushUrl pageState.key ("/e/" ++ event.id ++ "/edit")
          in ( format newState, newMsg )
        Err _ ->
          ( format Failure, Cmd.none )
    UpdateEditEventInput picker eventInput -> ( format (EditEventState (EditEvent Nothing { picker = picker, input = eventInput })), Cmd.none )
    EditEventOpenPicker ->
      case pageState.state of
        EditEvent _ { picker, input } ->
          let
            newPicker = DP.openPicker
                  (pickerSettings pageState.timeZone picker input)
                  input.startTime
                  (Just input.startTime)
                  picker

            newPageState =
              { state = EditEventState (EditEvent Nothing { input = input, picker = newPicker })
              , timeZone = pageState.timeZone
              , key = pageState.key
              , pageUrl = pageState.pageUrl
              }
          in ( newPageState, Cmd.none )
        _ -> ( format (EditEventState pageState.state), Cmd.none )
    SubmitEdit { picker, input } -> ( format (EditEventState (SubmittedEdit { picker = picker, input = input })), submitEdit input )
    EditedEvent result ->
      case result of
        Ok event ->
          let
            newState = format (ViewEventState (ViewEvent Nothing event (emptyAttendeeInput event.id)))
            cmd = Nav.pushUrl pageState.key ("/e/" ++ event.id)
          in ( newState, cmd )
        Err (Http.BadStatus 403) ->
          case pageState.state of
            SubmittedEdit state -> ( format (EditEventState (EditEvent (Just WrongPasswordModal) state)), Cmd.none )
            _ -> ( format Failure, Cmd.none )
        _ -> ( format Failure, Cmd.none )
    CloseEditEventModal ->
      case pageState.state of
        EditEvent _ input -> ( format (EditEventState (EditEvent Nothing input)), Cmd.none )
        otherState -> ( format (EditEventState otherState), Cmd.none )


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (EditEventMsg << LoadedEventForEdit) eventDecoder
        }

pickerSettings : Time.Zone -> DP.DatePicker -> EditEventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
  let
    getValueFromPicker : ( DP.DatePicker, Maybe Time.Posix ) -> Msg
    getValueFromPicker (dp, mTime) = case mTime of
                                             Nothing -> EditEventMsg (UpdateEditEventInput dp input)
                                             Just newStart -> EditEventMsg (UpdateEditEventInput dp { input | startTime = newStart })

  in DP.defaultSettings timeZone getValueFromPicker


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

