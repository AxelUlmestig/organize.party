port module Page.ViewEvent exposing
    ( ViewEventInput(..)
    , ViewEventMsg(..)
    , ViewEventState
    , handleSubscription
    , init
    , update
    , view
    )

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Navigation as Nav
import Date
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
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra
import Regex
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Shared.FormatUrls exposing (formatTextWithLinks)
import Shared.PageState exposing (PageState, setPageState)
import Shared.ViewAttendees exposing (viewAttendees)
import Shared.ViewComments exposing (viewComments)
import Time
import Tuple
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)


type ViewEventState
    = ViewEvent (Maybe ViewEventStateModal) Event AttendeeInput
    | AttendEventLoading
    | LoadingEvent
    | EventNotFound
    | Error


type ViewEventMsg
    = InternalMsg InternalMsg
    | ViewEditEventPage String


type InternalMsg
    = UpdateAttendeeInput AttendeeInput
    | AttendedEvent (Result Http.Error Event)
    | AttendMsg AttendeeInput
    | LoadedEvent (Result Http.Error Event)
    | CloseModal
    | RequestLocalStorageAttendeeInput String
    | CommentOnEvent AttendeeInput
    | CommentedOnEvent (Result Http.Error Event)
    | DoNothing


type ViewEventStateModal
    = InviteGuestsInfoModal
    | AttendeeSuccessModal


type ViewEventInput
    = ViewEventFromEvent Event
    | ViewEventFromId String


port writeToLocalStorage : Value -> Cmd msg


port requestLocalStorageAttendeeInput : String -> Cmd msg


port localStorageAttendeeInputReceiver : (Maybe String -> msg) -> Sub msg


init : ViewEventInput -> Bool -> ( ViewEventState, Cmd ViewEventMsg )
init viewEventInput newlyCreated =
    case viewEventInput of
        ViewEventFromId eventId ->
            let
                fetchEvent =
                    Http.get
                        { url = "/api/v1/events/" ++ eventId
                        , expect = Http.expectJson (InternalMsg << LoadedEvent) eventDecoder
                        }
            in
            ( LoadingEvent, fetchEvent )

        ViewEventFromEvent event ->
            let
                modal =
                    if newlyCreated then
                        Just InviteGuestsInfoModal

                    else
                        Nothing
            in
            ( ViewEvent modal event (emptyAttendeeInput event.id), Cmd.none )


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


view : PageState navbarState ViewEventState -> Html ViewEventMsg
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

        Error ->
            H.div [ A.class "center" ]
                [ H.text "Something went wrong, please try again later"
                ]

        ViewEvent maybeModal event attendeeInput ->
            let
                { id, title, description, startTime, endTime, location, attendees, comments } =
                    event

                onStatusUpdate newStatus =
                    InternalMsg <|
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
                        H.div [ A.class "modal-background" ]
                            [ H.div [ A.class "modal-window" ]
                                [ case modal of
                                    InviteGuestsInfoModal ->
                                        H.div []
                                            [ H.text "You have created a new event 🎉"
                                            , H.br [] []
                                            , H.text "Share this page with your friends to invite them."
                                            , H.div [ A.class "button-wrapper" ]
                                                [ H.button [ A.class "submit-button", onClick (InternalMsg CloseModal) ] [ H.text "Ok" ]
                                                ]
                                            ]

                                    AttendeeSuccessModal ->
                                        H.div []
                                            [ H.text "You have updated your status."
                                            , H.br [] []
                                            , H.text "Fill out the form again with the same email address to edit your status."
                                            , H.div [ A.class "button-wrapper" ]
                                                [ H.button [ A.class "submit-button", onClick (InternalMsg CloseModal) ] [ H.text "Ok" ]
                                                ]
                                            ]
                                ]
                            ]
                , H.div []
                    [ H.div [ A.style "display" "flex", A.style "justify-content" "space-between" ]
                        [ H.span [ A.attribute "data-testid" "view-event-title", A.class "event-title" ] [ H.text title ]
                        , H.a [ A.attribute "data-testid" "edit-event", A.href ("/e/" ++ id ++ "/edit"), A.style "display" "flex", A.style "align-items" "center", A.style "flex-direction" "column" ] [ Icon.view (Icon.styled [ Icon.lg, A.style "margin" "auto" ] Icon.pencil) ]
                        ]
                    , H.div [ A.attribute "data-testid" "view-event-description", A.style "white-space" "pre-wrap" ] [ formatTextWithLinks description ]
                    , H.div [ A.style "background-color" "white", borderRadius, A.style "box-shadow" "0px 0px 5px gray", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem", A.style "padding" "0.5rem" ]
                        [ H.div []
                            [ H.div [ A.style "margin-bottom" "1rem" ]
                                [ Icon.view (Icon.styled [ Icon.lg, A.style "margin-left" "0.5rem", A.style "margin-right" "0.5rem" ] Icon.locationDot)
                                , H.span [ A.attribute "data-testid" "view-event-location" ] [ H.text location ]
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
                    , H.div [] [ H.input [ A.attribute "data-testid" "view-event-attendee-name", A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> InternalMsg (UpdateAttendeeInput { attendeeInput | name = fn })), A.placeholder "Your name" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
                    , H.div [] [ H.input [ A.attribute "data-testid" "view-event-attendee-email", A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> InternalMsg (UpdateAttendeeInput { attendeeInput | email = e })), A.placeholder "Your email" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "plus one? ", H.input [ A.attribute "data-testid" "view-event-attendee-plus-one", A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> InternalMsg (UpdateAttendeeInput { attendeeInput | plusOne = po })) ] [] ]
                    , H.div
                        [ A.style "margin-top" "0.5rem", A.style "margin-bottom" "0.5rem" ]
                        [ H.text "get notified on comments? "
                        , H.input
                            [ A.type_ "checkbox"
                            , A.checked attendeeInput.getNotifiedOnComments
                            , onCheck (\gnoc -> InternalMsg (UpdateAttendeeInput { attendeeInput | getNotifiedOnComments = gnoc }))
                            ]
                            []
                        ]
                    , H.div []
                        [ H.select [ A.attribute "data-testid" "view-event-attendee-status", onInput onStatusUpdate ]
                            [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                            , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                            , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                            ]
                        ]
                    , H.div [ A.class "button-wrapper" ]
                        [ H.button
                            [ A.attribute "data-testid" "view-event-submit-attendee"
                            , disableUnlessValidInput attendeeInput
                            , onClick (InternalMsg (AttendMsg attendeeInput))
                            , A.class "submit-button"
                            ]
                            [ H.text "Submit"
                            ]
                        ]
                    ]
                , H.br [] []
                , H.br [] []
                , viewAttendees attendees
                , H.h1 [ A.class "mb-3" ] [ H.text "Comments" ]
                , addCommentView attendeeInput
                , viewComments pageState.currentTime comments
                ]


update : InternalMsg -> PageState navbarState ViewEventState -> ( PageState navbarState ViewEventState, Cmd ViewEventMsg )
update msg pageState =
    case msg of
        DoNothing ->
            ( pageState, Cmd.none )

        AttendedEvent result ->
            case result of
                Ok attendedEvent ->
                    ( setPageState (ViewEvent (Just AttendeeSuccessModal) attendedEvent (emptyAttendeeInput attendedEvent.id)) pageState, requestLocalStorageAttendeeInput attendedEvent.id )

                Err _ ->
                    ( setPageState Error pageState, Cmd.none )

        UpdateAttendeeInput input ->
            case pageState.state of
                ViewEvent modal event _ ->
                    let
                        newState =
                            setPageState (ViewEvent modal event input) pageState

                        localStorageObject =
                            Encode.object
                                [ ( "eventId", Encode.string input.eventId )
                                , ( "attendeeInput", encodeAttendeeInput input )
                                ]
                    in
                    ( newState, writeToLocalStorage localStorageObject )

                _ ->
                    ( pageState, Cmd.none )

        AttendMsg input ->
            ( setPageState AttendEventLoading pageState, attendEvent input )

        LoadedEvent result ->
            case result of
                Ok event ->
                    ( setPageState (ViewEvent Nothing event (emptyAttendeeInput event.id)) pageState, requestLocalStorageAttendeeInput event.id )

                Err (BadStatus 404) ->
                    ( setPageState EventNotFound pageState, Cmd.none )

                Err _ ->
                    ( setPageState Error pageState, Cmd.none )

        CloseModal ->
            case pageState.state of
                ViewEvent _ event attendeeInput ->
                    ( setPageState (ViewEvent Nothing event attendeeInput) pageState, Cmd.none )

                _ ->
                    ( pageState, Cmd.none )

        RequestLocalStorageAttendeeInput eventId ->
            ( pageState, requestLocalStorageAttendeeInput eventId )

        CommentOnEvent input ->
            -- can't think of a less messy way to delete the comment from the input state
            let
                localStorageObject =
                    Encode.object
                        [ ( "eventId", Encode.string input.eventId )
                        , ( "attendeeInput", encodeAttendeeInput { input | comment = "", forceNotificationOnComment = False } )
                        ]
            in
            ( setPageState AttendEventLoading pageState, Cmd.batch [ commentOnEvent input, writeToLocalStorage localStorageObject ] )

        CommentedOnEvent result ->
            case result of
                Ok attendedEvent ->
                    ( setPageState (ViewEvent Nothing attendedEvent (emptyAttendeeInput attendedEvent.id)) pageState, requestLocalStorageAttendeeInput attendedEvent.id )

                Err _ ->
                    ( setPageState Error pageState, Cmd.none )


handleSubscription : PageState navbarState ViewEventState -> Sub ViewEventMsg
handleSubscription _ =
    localStorageAttendeeInputReceiver <|
        \mJsonString ->
            case Maybe.map (Decode.decodeString attendeeInputDecoder) mJsonString of
                Just (Ok attendeeInput) ->
                    InternalMsg <| UpdateAttendeeInput attendeeInput

                _ ->
                    InternalMsg DoNothing


attendEvent : AttendeeInput -> Cmd ViewEventMsg
attendEvent input =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
        , expect = Http.expectJson (InternalMsg << AttendedEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInputForRsvp input)
        , timeout = Nothing
        , tracker = Nothing
        }


commentOnEvent : AttendeeInput -> Cmd ViewEventMsg
commentOnEvent input =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/comment"
        , expect = Http.expectJson (InternalMsg << CommentedOnEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInputForComment input)
        , timeout = Nothing
        , tracker = Nothing
        }


addCommentView : AttendeeInput -> Html ViewEventMsg
addCommentView attendeeInput =
    H.map InternalMsg <|
        H.div []
            [ H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Name" ]
            , H.div [] [ H.input [ A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> UpdateAttendeeInput { attendeeInput | name = fn }), A.placeholder "Your name" ] [] ]
            , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
            , H.div [] [ H.input [ A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> UpdateAttendeeInput { attendeeInput | email = e }), A.placeholder "Your email" ] [] ]
            , H.div [ A.class "submit-comment-button" ] [ H.text "Comment" ]
            , expandingTextarea
                { text = attendeeInput.comment
                , onInput = \c -> UpdateAttendeeInput { attendeeInput | comment = c }
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
                    ]
                    []
                ]
            , H.div [ A.class "button-wrapper" ]
                [ H.button [ A.class "submit-button", disableUnlessValidCommentInput attendeeInput, onClick (CommentOnEvent attendeeInput) ] [ H.text "Comment" ]
                ]
            ]
