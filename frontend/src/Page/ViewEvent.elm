port module Page.ViewEvent exposing (
    update,
    view,
    handleSubscription,
    init,
    ViewEventInput(..),
    ViewEventMsg(..),
    ViewEventState
  )

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

type ViewEventState
    = ViewEvent (Maybe ViewEventStateModal) Event AttendeeInput
    | AttendEventLoading
    | LoadingEvent
    | EventNotFound
    | ViewEventError

type ViewEventMsg
    = ViewEventInternalMsg ViewEventInternalMsg
    | ViewEditEventPage String

type ViewEventInternalMsg
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

type alias AttendeeInput =
    { eventId : String
    , email : String
    , name : String
    , status : AttendeeStatus
    , plusOne : Bool
    , getNotifiedOnComments : Bool
    , comment : String
    , forceNotificationOnComment : Bool
    }

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
        let fetchEvent =
              Http.get
                  { url = "/api/v1/events/" ++ eventId
                  , expect = Http.expectJson (ViewEventInternalMsg << LoadedEvent) eventDecoder
                  }
        in ( LoadingEvent, fetchEvent )
      ViewEventFromEvent event ->
        let modal = if newlyCreated then Just InviteGuestsInfoModal else Nothing
        in ( ViewEvent modal event (emptyAttendeeInput event.id), Cmd.none )

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

        ViewEventError ->
            H.div [ A.class "center" ]
              [ H.text "Something went wrong, please try again later"
              ]

        ViewEvent maybeModal event attendeeInput ->
            let
                { id, title, description, startTime, endTime, location, attendees, comments } =
                    event

                onStatusUpdate newStatus =
                    ViewEventInternalMsg <|
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
                                            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick (ViewEventInternalMsg CloseModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
                                                ]
                                            ]

                                    AttendeeSuccessModal ->
                                        H.div []
                                            [ H.text "You have updated your status."
                                            , H.br [] []
                                            , H.text "Fill in the form again with the same email address to edit your status."
                                            , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                                                [ H.button [ A.style "background-color" "#1c2c3b", onClick (ViewEventInternalMsg CloseModal), A.class "btn btn-primary" ] [ H.text "Ok" ]
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
                    , H.div [] [ H.input [ A.attribute "data-testid" "view-event-attendee-name", A.class "padded-input", A.attribute "autocomplete" "name", A.style "width" "100%", borderRadius, A.value attendeeInput.name, onInput (\fn -> ViewEventInternalMsg (UpdateAttendeeInput { attendeeInput | name = fn })), A.placeholder "Your name" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "Email" ]
                    , H.div [] [ H.input [ A.attribute "data-testid" "view-event-attendee-email", A.class "padded-input", A.attribute "type" "email", A.attribute "autocomplete" "email", A.style "width" "100%", borderRadius, A.value attendeeInput.email, onInput (\e -> ViewEventInternalMsg (UpdateAttendeeInput { attendeeInput | email = e })), A.placeholder "Your email" ] [] ]
                    , H.div [ A.style "margin-top" "0.5rem" ] [ H.text "plus one? ", H.input [ A.attribute "data-testid" "view-event-attendee-plus-one", A.type_ "checkbox", A.checked attendeeInput.plusOne, onCheck (\po -> ViewEventInternalMsg (UpdateAttendeeInput { attendeeInput | plusOne = po })) ] [] ]
                    , H.div
                      [ A.style "margin-top" "0.5rem", A.style "margin-bottom" "0.5rem" ]
                      [ H.text "get notified on comments? ",
                      H.input
                        [ A.type_ "checkbox"
                        , A.checked attendeeInput.getNotifiedOnComments
                        , onCheck (\gnoc -> ViewEventInternalMsg (UpdateAttendeeInput { attendeeInput | getNotifiedOnComments = gnoc }))
                        ] []
                      ]
                    , H.div []
                        [ H.select [ A.attribute "data-testid" "view-event-attendee-status", onInput onStatusUpdate ]
                            [ H.option [ A.selected (attendeeInput.status == Coming) ] [ H.text "Coming" ]
                            , H.option [ A.selected (attendeeInput.status == MaybeComing) ] [ H.text "Maybe Coming" ]
                            , H.option [ A.selected (attendeeInput.status == NotComing) ] [ H.text "Not Coming" ]
                            ]
                        ]
                    , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                        [ H.button [ A.attribute "data-testid" "view-event-submit-attendee", A.style "background-color" "#1c2c3b", disableUnlessValidInput attendeeInput, onClick (ViewEventInternalMsg (AttendMsg attendeeInput)), A.class "btn btn-primary" ] [ H.text "Submit" ]
                        ]
                    ]
                , H.br [] []
                , H.br [] []
                , viewAttendees attendees
                , H.h1 [ A.class "mb-3" ] [ H.text "Comments" ]
                , addCommentView attendeeInput
                , viewComments pageState.currentTime comments
                ]


update : ViewEventInternalMsg -> PageState navbarState ViewEventState -> ( PageState navbarState ViewEventState, Cmd ViewEventMsg )
update msg pageState =
    case msg of
        DoNothing -> ( pageState, Cmd.none )
        AttendedEvent result ->
            case result of
                Ok attendedEvent ->

                    let pushUrlCmd = Nav.pushUrl pageState.key ("/e/" ++ attendedEvent.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput attendedEvent.id, pushUrlCmd]
                    in ( setPageState (ViewEvent (Just AttendeeSuccessModal) attendedEvent (emptyAttendeeInput attendedEvent.id)) pageState, cmds )

                Err _ ->
                    ( setPageState ViewEventError pageState, Cmd.none )

        UpdateAttendeeInput input ->
            case pageState.state of
                ViewEvent modal event _ ->
                    let newState = setPageState (ViewEvent modal event input) pageState
                        localStorageObject =
                          Encode.object
                            [ ("eventId", Encode.string input.eventId)
                            , ("attendeeInput", encodeAttendeeInput input)
                            ]
                    in ( newState, writeToLocalStorage localStorageObject )

                _ ->
                    ( pageState, Cmd.none )

        AttendMsg input ->
            ( setPageState AttendEventLoading pageState, attendEvent input )

        LoadedEvent result ->
            case result of
                Ok event ->
                    let pushUrlCmd = Nav.pushUrl pageState.key ("/e/" ++ event.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput event.id, pushUrlCmd]
                    in ( setPageState (ViewEvent Nothing event (emptyAttendeeInput event.id)) pageState, cmds )

                Err (BadStatus 404) ->
                    ( setPageState EventNotFound pageState, Cmd.none )

                Err _ ->
                    ( setPageState ViewEventError pageState, Cmd.none )

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
            let localStorageObject =
                    Encode.object
                      [ ("eventId", Encode.string input.eventId)
                      , ("attendeeInput", encodeAttendeeInput { input | comment = "", forceNotificationOnComment = False })
                      ]
            in ( setPageState AttendEventLoading pageState, Cmd.batch [ commentOnEvent input, writeToLocalStorage localStorageObject ])

        CommentedOnEvent result ->
            case result of
                Ok attendedEvent ->

                    let pushUrlCmd = Nav.pushUrl pageState.key ("/e/" ++ attendedEvent.id)
                        cmds = Cmd.batch [requestLocalStorageAttendeeInput attendedEvent.id, pushUrlCmd]
                    in ( setPageState (ViewEvent Nothing attendedEvent (emptyAttendeeInput attendedEvent.id)) pageState, cmds )

                Err _ ->
                    ( setPageState ViewEventError pageState, Cmd.none )


handleSubscription : PageState navbarState ViewEventState -> Sub ViewEventMsg
handleSubscription _ =
  localStorageAttendeeInputReceiver
    <| \mJsonString -> case Maybe.map (Decode.decodeString attendeeInputDecoder) mJsonString of
      Just (Ok attendeeInput) -> ViewEventInternalMsg <| UpdateAttendeeInput attendeeInput
      _ -> ViewEventInternalMsg DoNothing

attendEvent : AttendeeInput -> Cmd ViewEventMsg
attendEvent input =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/events/" ++ input.eventId ++ "/attend"
        , expect = Http.expectJson (ViewEventInternalMsg << AttendedEvent) eventDecoder
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
        , expect = Http.expectJson (ViewEventInternalMsg << CommentedOnEvent) eventDecoder
        , body = Http.jsonBody (encodeAttendeeInputForComment input)
        , timeout = Nothing
        , tracker = Nothing
        }

addCommentView : AttendeeInput -> Html ViewEventMsg
addCommentView attendeeInput =
  H.map ViewEventInternalMsg <|
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

emptyAttendeeInput : String -> AttendeeInput
emptyAttendeeInput eventId =
    { eventId = eventId
    , email = ""
    , name = ""
    , status = Coming
    , plusOne = False
    , getNotifiedOnComments = False
    , comment = ""
    , forceNotificationOnComment = False
    }

encodeAttendeeInput : AttendeeInput -> Value
encodeAttendeeInput { eventId, email, name, status, plusOne, getNotifiedOnComments, comment, forceNotificationOnComment } =
    let
        encodeAttendeeStatus ai =
            case ai of
                Coming ->
                    Encode.string "Coming"

                MaybeComing ->
                    Encode.string "MaybeComing"

                NotComing ->
                    Encode.string "NotComing"
    in
    Encode.object
        [ ( "eventId", Encode.string eventId )
        , ( "email", Encode.string (String.trim email) )
        , ( "name", Encode.string (String.trim name) )
        , ( "status", encodeAttendeeStatus status )
        , ( "plusOne", Encode.bool plusOne )
        , ( "getNotifiedOnComments", Encode.bool getNotifiedOnComments )
        , ( "comment", Encode.string comment )
        , ( "forceNotificationOnComment", Encode.bool forceNotificationOnComment )
        ]

encodeAttendeeInputForRsvp : AttendeeInput -> Value
encodeAttendeeInputForRsvp { eventId, email, name, status, plusOne, getNotifiedOnComments } =
    let
        encodeAttendeeStatus ai =
            case ai of
                Coming ->
                    Encode.string "Coming"

                MaybeComing ->
                    Encode.string "MaybeComing"

                NotComing ->
                    Encode.string "NotComing"
    in
    Encode.object
        [ ( "eventId", Encode.string eventId )
        , ( "email", Encode.string (String.trim email) )
        , ( "name", Encode.string (String.trim name) )
        , ( "status", encodeAttendeeStatus status )
        , ( "plusOne", Encode.bool plusOne )
        , ( "getNotifiedOnComments", Encode.bool getNotifiedOnComments )
        ]

encodeAttendeeInputForComment : AttendeeInput -> Value
encodeAttendeeInputForComment { eventId, email, name, comment, forceNotificationOnComment } =
  Encode.object
    [ ( "eventId", Encode.string eventId )
    , ( "email", Encode.string (String.trim email) )
    , ( "name", Encode.string (String.trim name) )
    , ( "comment", Encode.string (String.trim comment) )
    , ( "forceNotificationOnComment", Encode.bool forceNotificationOnComment )
    ]

attendeeInputDecoder : D.Decoder AttendeeInput
attendeeInputDecoder =
    D.map8 AttendeeInput
        (D.field "eventId" D.string)
        (D.field "email" D.string)
        (D.field "name" D.string)
        (D.field "status" attendeeStatusDecoder)
        (D.field "plusOne" D.bool)
        (D.map (Maybe.withDefault False) <| D.maybe <| D.field "getNotifiedOnComments" D.bool)
        (D.field "comment" D.string)
        (D.map (Maybe.withDefault False) <| D.maybe <| D.field "forceNotificationOnComment" D.bool)

attendeeStatusDecoder : D.Decoder AttendeeStatus
attendeeStatusDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Coming" ->
                        D.succeed Coming

                    "MaybeComing" ->
                        D.succeed MaybeComing

                    "NotComing" ->
                        D.succeed NotComing

                    somethingElse ->
                        D.fail ("Unknown status: " ++ somethingElse)
            )


attendeeStatusToString : AttendeeStatus -> String
attendeeStatusToString status =
    case status of
        Coming ->
            "Coming"

        MaybeComing ->
            "Maybe Coming"

        NotComing ->
            "Not Coming"
