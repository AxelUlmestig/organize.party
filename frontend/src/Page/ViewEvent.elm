port module Page.ViewEvent exposing (fetchEvent, update, view, handleSubscription)

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
import Shared.ViewComments exposing (viewComments)
import Time as Time
import Tuple
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)
import Shared.FormatUrls exposing (formatTextWithLinks)
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Json.Decode as Decode

port writeToLocalStorage : Value -> Cmd msg
port requestLocalStorageAttendeeInput : String -> Cmd msg
port localStorageAttendeeInputReceiver : (Maybe String -> msg) -> Sub msg

borderRadius =
    A.style "border-radius" "5px"


-- TODO: refactor these checks into something nicer
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

disableUnlessValidCommentInput { email, name, comment } =
    let
        validEmailRegex =
            "^ *[a-zA-Z0-9.!#$%&''*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)* *$"

        mRegex =
            Regex.fromString validEmailRegex

        mContains =
            Maybe.map (\regex -> Regex.contains regex email) mRegex
    in
    A.disabled (mContains /= Just True || name == "" || comment == "")


fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (ViewEventMsg << LoadedEvent) eventDecoder
        }


view : PageState ViewEventState -> Html ViewEventMsg
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
                { id, title, description, startTime, endTime, location, attendees, comments } =
                    event

                onStatusUpdate newStatus =
                    case newStatus of
                        "Coming" ->
                            UpdateAttendeeInput { attendeeInput | status = Coming }

                        "Maybe Coming" ->
                            UpdateAttendeeInput { attendeeInput | status = MaybeComing }

                        "Not Coming" ->
                            UpdateAttendeeInput { attendeeInput | status = NotComing }

                        _ ->
                            UpdateAttendeeInput attendeeInput
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
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick CloseModal, A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]

                                    AttendeeSuccessModal ->
                                        H.div []
                                            [ H.text "You have updated your status."
                                            , H.br [] []
                                            , H.text "Fill in the form again with the same email address to edit your status."
                                            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick CloseModal, A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.div []
                    [ H.div [ A.style "display" "flex", A.style "justify-content" "space-between" ]
                        [ H.span [ A.class "event-title" ] [ H.text title ]
                        , H.a [ A.href ("/e/" ++ id ++ "/edit"), A.style "display" "flex", A.style "align-items" "center", A.style "flex-direction" "column" ] [ Icon.view (Icon.styled [ Icon.lg, A.style "margin" "auto" ] Icon.pencil) ]
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
                    , H.div [] [ H.input [ A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> UpdateAttendeeInput { attendeeInput | name = fn }), A.placeholder "Your name" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
                    , H.div [] [ H.input [ A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> UpdateAttendeeInput { attendeeInput | email = e }), A.placeholder "Your email" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "plus one? ", H.input [ A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> UpdateAttendeeInput { attendeeInput | plusOne = po }) ] [] ]
                    , H.div
                      [ A.style "margin-top" "0.5rem", A.style "margin-bottom" "0.5rem" ]
                      [ H.text "get notified on comments? ",
                      H.input
                        [ A.type_ "checkbox"
                        , A.checked attendeeInput.getNotifiedOnComments
                        , onCheck (\gnoc -> UpdateAttendeeInput { attendeeInput | getNotifiedOnComments = gnoc })
                        ] []
                      ]
                    , H.div []
                        [ H.select [ onInput onStatusUpdate ]
                            [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                            , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                            , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                            ]
                        ]
                    , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                        [ H.button [ A.style "background-color" "#1c2c3b", disableUnlessValidInput attendeeInput, onClick (AttendMsg attendeeInput), A.class "btn btn-primary" ] [ H.text "Submit" ]
                        ]
                    ]
                , H.br [] []
                , H.br [] []
                , viewAttendees attendees
                , H.h1 [ A.class "mb-3" ] [ H.text "Comments" ]
                , addCommentView attendeeInput
                , viewComments pageState.currentTime comments
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

                    let pushUrlCmd = Nav.pushUrl key ("/e/" ++ attendedEvent.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput attendedEvent.id, pushUrlCmd]
                    in ( format (ViewEventState (ViewEvent (Just AttendeeSuccessModal) attendedEvent (emptyAttendeeInput attendedEvent.id))), cmds )

                Err _ ->
                    ( format Failure, Cmd.none )

        UpdateAttendeeInput input ->
            case pageState.state of
                ViewEvent modal event _ ->
                    let newState = format (ViewEventState (ViewEvent modal event input))
                        localStorageObject =
                          Encode.object
                            [ ("eventId", Encode.string input.eventId)
                            , ("attendeeInput", encodeAttendeeInput input)
                            ]
                    in ( newState, writeToLocalStorage localStorageObject )

                _ ->
                    ( format (ViewEventState pageState.state), Cmd.none )

        AttendMsg input ->
            ( format (ViewEventState AttendEventLoading), attendEvent input )

        LoadedEvent result ->
            case result of
                Ok event ->
                    let pushUrlCmd = Nav.pushUrl pageState.key ("/e/" ++ event.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput event.id, pushUrlCmd]
                    in ( format (ViewEventState (ViewEvent Nothing event (emptyAttendeeInput event.id))), cmds )

                Err (BadStatus 404) ->
                    ( format (ViewEventState EventNotFound), Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        CloseModal ->
            case state of
                ViewEvent _ event attendeeInput ->
                    ( format (ViewEventState (ViewEvent Nothing event attendeeInput)), Cmd.none )

                otherState ->
                    ( format (ViewEventState otherState), Cmd.none )

        RequestLocalStorageAttendeeInput eventId ->
            ( format (ViewEventState state), requestLocalStorageAttendeeInput eventId )

        CommentOnEvent input ->
            -- can't think of a less messy way to delete the comment from the input state
            let localStorageObject =
                    Encode.object
                      [ ("eventId", Encode.string input.eventId)
                      , ("attendeeInput", encodeAttendeeInput { input | comment = "", forceNotificationOnComment = False })
                      ]
            in ( format (ViewEventState AttendEventLoading), Cmd.batch [ commentOnEvent input, writeToLocalStorage localStorageObject ])

        CommentedOnEvent result ->
            case result of
                Ok attendedEvent ->

                    let pushUrlCmd = Nav.pushUrl key ("/e/" ++ attendedEvent.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput attendedEvent.id, pushUrlCmd]
                    in ( format (ViewEventState (ViewEvent Nothing attendedEvent (emptyAttendeeInput attendedEvent.id))), cmds )

                Err _ ->
                    ( format Failure, Cmd.none )


handleSubscription : PageState ViewEventState -> Sub Msg
handleSubscription _ =
  localStorageAttendeeInputReceiver
    <| \mJsonString -> case Maybe.map (Decode.decodeString attendeeInputDecoder) mJsonString of
      Just (Ok attendeeInput) -> ViewEventMsg <| UpdateAttendeeInput attendeeInput
      _ -> DoNothing

attendEvent : AttendeeInput -> Cmd Msg
attendEvent input =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
        , expect = Http.expectJson (ViewEventMsg << AttendedEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInputForRsvp input)
        , timeout = Nothing
        , tracker = Nothing
        }

commentOnEvent : AttendeeInput -> Cmd Msg
commentOnEvent input =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/comment"
        , expect = Http.expectJson (ViewEventMsg << CommentedOnEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInputForComment input)
        , timeout = Nothing
        , tracker = Nothing
        }

addCommentView : AttendeeInput -> Html ViewEventMsg
addCommentView attendeeInput =
  H.div []
    [ H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Name" ]
    , H.div [] [ H.input [ A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> UpdateAttendeeInput { attendeeInput | name = fn }), A.placeholder "Your name" ] [] ]
    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
    , H.div [] [ H.input [ A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> UpdateAttendeeInput { attendeeInput | email = e }), A.placeholder "Your email" ] [] ]
    , H.div [ A.class "submit-comment-button" ] [ H.text "Comment" ]
    , expandingTextarea
        { text = attendeeInput.comment
        , onInput = (\c -> UpdateAttendeeInput { attendeeInput | comment = c })
        , placeholder = "Leave a comment"
        , styling = []
        }
    , H.div
      [ A.style "margin-top" "0.5rem", A.style "margin-bottom" "0.5rem" ]
      [ H.text "send email notification to everyone? "
      , H.input
        [ A.type_ "checkbox"
        , A.checked attendeeInput.forceNotificationOnComment
        , onCheck (\fnoc -> UpdateAttendeeInput { attendeeInput | forceNotificationOnComment = fnoc })
        ] []
      ]
    , H.div [ A.class "text-center", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
        [ H.button [ A.style "background-color" "#1c2c3b", disableUnlessValidCommentInput attendeeInput, onClick (CommentOnEvent attendeeInput), A.class "btn btn-primary" ] [ H.text "Comment" ]
        ]
    ]
