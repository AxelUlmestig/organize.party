module Page.ViewEvent exposing (view, update)

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
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Regex

import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)

borderRadius = A.style "border-radius" "5px"

disableUnlessValidInput { email, name } =
  let
      validEmailRegex = "^ *[a-zA-Z0-9.!#$%&''*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)* *$"
      mRegex = Regex.fromString validEmailRegex

      mContains = Maybe.map (\regex -> Regex.contains regex email) mRegex
  in A.disabled (mContains /= Just True || name == "")

view : PageState ViewEventState -> Html Msg
view pageState =
  case pageState.state of
    AttendEventLoading -> H.text "Loading event..."
    LoadingEvent -> H.text "Loading event..."
    ViewEvent maybeModal event attendeeInput ->
      let
        { title, description, startTime, endTime, location, attendees } = event

        onStatusUpdate newStatus = ViewEventMsg (case newStatus of
                                    "Coming" -> UpdateAttendeeInput { attendeeInput | status = Coming }
                                    "Maybe Coming" -> UpdateAttendeeInput { attendeeInput | status = MaybeComing }
                                    "Not Coming" -> UpdateAttendeeInput { attendeeInput | status = NotComing }
                                    _ -> UpdateAttendeeInput attendeeInput)

      in
        H.div []
          [ case maybeModal of
              Nothing -> H.span [] []
              Just modal ->
                H.div [ A.class "modal" ]
                  [ H.div [ A.class "modal_window" ]
                    [ case modal of
                        InviteGuestsInfoModal ->
                          H.div []
                          [ H.text "You have created a new event 🎉"
                          , H.br [] []
                          , H.text "Share this page with your friends to invite them."
                          , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                            [ H.button [ A.style "background-color" "#1c2c3b", onClick (ViewEventMsg CloseModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                            ]
                          ]
                        AttendeeSuccessModal ->
                          H.div []
                          [ H.text "You have updated your status."
                          , H.br [] []
                          , H.text "Fill in the form again with the same email address to edit your status."
                          , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                            [ H.button [ A.style "background-color" "#1c2c3b", onClick (ViewEventMsg CloseModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                            ]
                          ]
                    ]
                  ]

          , H.div []
              [ H.div [] [ H.h1 [] [ H.text title ] ]
              , H.div [] [ H.text description ]
              , H.div [ A.style "background-color" "white", borderRadius, A.style "box-shadow" "0px 0px 5px gray", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem", A.style "padding" "0.5rem" ]
                [ H.div []
                  [ H.div [ A.style "margin-bottom" "1rem" ]
                    [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem"  ] Icon.locationDot)
                    , H.text location
                    ]
                    , H.div [ A.style "display" "flex", A.style "justify-content" "space-between", A.style "margin-top" "1rem" ]
                      [ H.span [ A.style "flex" "1" ]
                        [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem"  ] Icon.calendar)
                        , H.text (viewEventDate pageState.timeZone startTime)
                        ]
                      , H.span [ A.style "flex" "1" ]
                        [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem"  ] Icon.clock)
                        , H.text (viewEventTime pageState.timeZone startTime)
                        ]
                      ]
                  ]
                ]
              ]

          , H.br [] []
          , H.div []
              [ H.b [] [ H.text "Are you attending?" ]

              , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
              , H.div [] [ H.input [ A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | email = e })), A.placeholder "Your email" ] [] ]

              , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Name" ]
              , H.div [] [ H.input [ A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | name = fn })), A.placeholder "Your name" ] [] ]

              , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "plus one? ", H.input [ A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | plusOne = po })) ] [] ]
              , H.div []
                  [ H.select [ onInput onStatusUpdate ]
                      [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                      , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                      , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                      ]
                  ]
              , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                [ H.button [ A.style "background-color" "#1c2c3b", disableUnlessValidInput attendeeInput, onClick (ViewEventMsg (AttendMsg attendeeInput)), A.class "btn btn-primary" ] [ H.text "Submit" ]
                ]
              ]
          , H.br [] []
          , H.h3 [] [ H.text "Attendees" ]

          , H.div [ A.style "width" "100%" ]
            ( H.div [ A.style "display" "flex", A.style "width" "100%" ]
              [ H.span [ A.style "flex" "1", A.style "font-weight" "bold" ] [ H.text "Name" ]
              , H.span [ A.style "flex" "1", A.style "font-weight" "bold" ] [ H.text "Coming?" ]
              , H.span [ A.style "flex" "1", A.style "font-weight" "bold" ] [ H.text "Plus One?" ]
              ]
              :: List.map (\{name, status, plusOne} ->
                H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex" "1" ] [ H.text name ]
                , H.span [ A.style "flex" "1" ] [ H.text (attendeeStatusToString status) ]
                , H.span [ A.style "flex" "1" ] [ H.text (if plusOne then "Yes" else "No") ]
                ]
                ) attendees
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
          ( format (ViewEventState (ViewEvent (Just AttendeeSuccessModal) attendedEvent (emptyAttendeeInput attendedEvent.id))), Nav.pushUrl key ("/e/" ++ attendedEvent.id) )
        Err _ ->
          ( format Failure, Cmd.none )

    UpdateAttendeeInput input ->
      case pageState.state of
        ViewEvent modal event _ -> ( format (ViewEventState (ViewEvent modal event input)), Cmd.none )
        _ -> ( format (ViewEventState pageState.state), Cmd.none )

    AttendMsg input -> ( format (ViewEventState AttendEventLoading), attendEvent input )

    LoadedEvent result ->
        case result of
            Ok event ->
                ( format (ViewEventState (ViewEvent Nothing event (emptyAttendeeInput event.id))), Nav.pushUrl pageState.key ("/e/" ++ event.id) )
            Err _ ->
                ( format Failure, Cmd.none )

    CloseModal ->
      case state of
        ViewEvent _ event attendeeInput -> ( format (ViewEventState (ViewEvent Nothing event attendeeInput)), Cmd.none )
        otherState -> ( format (ViewEventState otherState), Cmd.none )


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

