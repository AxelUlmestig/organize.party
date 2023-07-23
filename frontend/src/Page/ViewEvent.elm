module Page.ViewEvent exposing (fetchEvent, update, view)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Navigation as Nav
import Date as Date
import DurationDatePicker as DP
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http exposing (Error(..))
import Iso8601 as Iso8601
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import List.Extra
import Regex
import Shared.ViewAttendees exposing (viewAttendees)
import Time as Time
import Tuple
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)
import Shared.FormatUrls exposing (formatTextWithLinks)

borderRadius =
    A.style "border-radius" "5px"


disableUnlessValidInput { email, name } =
    let
        validEmailRegex =
            "^ *[a-zA-Z0-9.!#$%&''*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)* *$"

        mRegex =
            Regex.fromString validEmailRegex

        mContains =
            Maybe.map (\regex -> Regex.contains regex email) mRegex
    in
    A.disabled (mContains /= Just True || name == "")


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (ViewEventMsg << LoadedEvent) eventDecoder
        }


view : PageState ViewEventState -> Html Msg
view pageState =
    case pageState.state of
        AttendEventLoading ->
            H.div [ A.class "center" ]
              [ H.text "Loading event..."
              ]

        LoadingEvent ->
            H.div [ A.class "center" ]
              [ H.text "Loading event..."
              ]

        EventNotFound ->
            H.div [ A.class "center" ]
              [ H.text "Event not found, please verify the URL"
              ]

        ViewEvent maybeModal event attendeeInput ->
            let
                { id, title, description, startTime, endTime, location, attendees } =
                    event

                onStatusUpdate newStatus =
                    ViewEventMsg
                        (case newStatus of
                            "Coming" ->
                                UpdateAttendeeInput { attendeeInput | status = Coming }

                            "Maybe Coming" ->
                                UpdateAttendeeInput { attendeeInput | status = MaybeComing }

                            "Not Coming" ->
                                UpdateAttendeeInput { attendeeInput | status = NotComing }

                            _ ->
                                UpdateAttendeeInput attendeeInput
                        )
            in
            H.div []
                [ case maybeModal of
                    Nothing ->
                        H.span [] []

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

                                    AttendeeCommentModal name comment ->
                                        H.div []
                                            [ H.b [] [ H.text name ]
                                            , H.text " says"
                                            , H.br [] []
                                            , H.br [] []
                                            , H.div [ A.style "white-space" "pre-wrap" ] [ formatTextWithLinks comment ]
                                            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick (ViewEventMsg CloseModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.div []
                    [ H.div [ A.style "display" "flex", A.style "justify-content" "space-between" ]
                        [ H.h1 [] [ H.text title ]
                        , H.a [ A.href ("/e/" ++ id ++ "/edit"), A.style "display" "flex", A.style "align-items" "center", A.style "flex-direction" "column" ] [ Icon.view (Icon.styled [ Icon.lg, A.style "margin" "auto" ] Icon.wrench) ]
                        ]
                    , H.div [ A.style "white-space" "pre-wrap" ] [ formatTextWithLinks description ]
                    , H.div [ A.style "background-color" "white", borderRadius, A.style "box-shadow" "0px 0px 5px gray", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem", A.style "padding" "0.5rem" ]
                        [ H.div []
                            [ H.div [ A.style "margin-bottom" "1rem" ]
                                [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem" ] Icon.locationDot)
                                , H.text location
                                ]
                            , H.div [ A.style "display" "flex", A.style "justify-content" "space-between", A.style "margin-top" "1rem" ]
                                [ H.span [ A.style "flex" "1" ]
                                    [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem" ] Icon.calendar)
                                    , H.text (viewEventDate pageState.timeZone startTime)
                                    ]
                                , H.span [ A.style "flex" "1" ]
                                    [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem" ] Icon.clock)
                                    , H.text (viewEventTime pageState.timeZone startTime)
                                    ]
                                ]
                            ]
                        ]
                    ]
                , H.br [] []
                , H.div []
                    [ H.b [] [ H.text "Are you attending?" ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Name" ]
                    , H.div [] [ H.input [ A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | name = fn })), A.placeholder "Your name" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
                    , H.div [] [ H.input [ A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> ViewEventMsg (UpdateAttendeeInput { attendeeInput | email = e })), A.placeholder "Your email" ] [] ]
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
                , H.br [] []
                , viewAttendees attendees
                ]


update : ViewEventMsg -> PageState ViewEventState -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
        AttendedEvent result ->
            case result of
                Ok attendedEvent ->
                    ( format (ViewEventState (ViewEvent (Just AttendeeSuccessModal) attendedEvent (emptyAttendeeInput attendedEvent.id))), Nav.pushUrl key ("/e/" ++ attendedEvent.id) )

                Err _ ->
                    ( format Failure, Cmd.none )

        UpdateAttendeeInput input ->
            case pageState.state of
                ViewEvent modal event _ ->
                    ( format (ViewEventState (ViewEvent modal event input)), Cmd.none )

                _ ->
                    ( format (ViewEventState pageState.state), Cmd.none )

        AttendMsg input ->
            ( format (ViewEventState AttendEventLoading), attendEvent input )

        LoadedEvent result ->
            case result of
                Ok event ->
                    ( format (ViewEventState (ViewEvent Nothing event (emptyAttendeeInput event.id))), Nav.pushUrl pageState.key ("/e/" ++ event.id) )

                Err (BadStatus 404) ->
                    ( format (ViewEventState EventNotFound), Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        DisplayComment name comment ->
            case pageState.state of
                ViewEvent _ event attendeeInput -> ( format (ViewEventState (ViewEvent (Just (AttendeeCommentModal name comment)) event attendeeInput)), Cmd.none )
                otherState ->
                    ( format (ViewEventState otherState), Cmd.none )

        CloseModal ->
            case state of
                ViewEvent _ event attendeeInput ->
                    ( format (ViewEventState (ViewEvent Nothing event attendeeInput)), Cmd.none )

                otherState ->
                    ( format (ViewEventState otherState), Cmd.none )


attendEvent : AttendeeInput -> Cmd Msg
attendEvent input =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
        , expect = Http.expectJson (ViewEventMsg << AttendedEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInput input)
        , timeout = Nothing
        , tracker = Nothing
        }
