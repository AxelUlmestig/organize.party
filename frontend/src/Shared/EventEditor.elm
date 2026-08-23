module Shared.EventEditor exposing
    ( Msg(..)
    , State
    , handleSubscription
    , init
    , prepareInput
    , update
    , view
    , viewPhotoUploadStatus
    )

import Browser.Dom as Dom
import Dict exposing (Dict)
import File exposing (File)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Process
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Shared.PhotoUploader as Photo
import Shared.SectionSeparator exposing (sectionSeparator)
import SingleDatePicker as DP
import Task
import Time
import Types exposing (EventInput, Photo, emptyEventInput)
import Util exposing (viewEventDate, viewEventTime)


type alias State =
    { picker : DP.DatePicker
    , input : EventInput
    , timezone : Time.Zone
    , photoUploader : Photo.State
    , maybeModal : Maybe EventEditorModal
    }


type EventEditorModal
    = PhotoUploadFailure String


type Msg
    = InternalMsg InternalMsg
    | EventInputReady EventInput


type InternalMsg
    = UpdateEventInput DP.DatePicker EventInput
    | UpdateEventStartTime ( DP.DatePicker, Maybe Time.Posix )
    | OpenPicker
    | FocusTimePicker
    | FocusTimePickerSoon
    | DoNothing
    | PhotoUploaderMsg Photo.Msg
    | CloseModal


init : Time.Zone -> EventInput -> Maybe Photo -> ( State, Cmd Msg )
init timezone eventInput photo =
    let
        ( photoState, photoMsg ) =
            Photo.init photo
    in
    ( { timezone = timezone, picker = DP.init, input = eventInput, photoUploader = photoState, maybeModal = Nothing }
    , Cmd.map (InternalMsg << PhotoUploaderMsg) photoMsg
    )


viewPhotoUploadStatus : State -> Html a
viewPhotoUploadStatus state =
    Photo.view state.photoUploader


view : Dict String String -> State -> Html Msg
view copy { picker, input, timezone, photoUploader, maybeModal } =
    let
        updatePicker : EventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> Msg
        updatePicker input2 ( picker2, mTimestamp ) =
            case mTimestamp of
                Just newStart ->
                    InternalMsg (UpdateEventInput picker2 { input2 | startTime = newStart })

                Nothing ->
                    InternalMsg (UpdateEventInput picker2 input2)
    in
    H.div []
        [ case maybeModal of
            Nothing ->
                H.text ""

            Just modal ->
                H.div [ A.class "modal-background" ]
                    [ H.div [ A.class "modal-window" ]
                        [ case modal of
                            PhotoUploadFailure errorMessage ->
                                H.div []
                                    [ H.div [] [ H.text "Something went wrong when uploading photo" ]
                                    , H.div [] [ H.text errorMessage ]
                                    , H.div [ A.class "button-wrapper" ]
                                        [ H.button [ A.class "submit-button", onClick (InternalMsg CloseModal) ] [ H.text "Ok" ]
                                        ]
                                    ]
                        ]
                    ]
        , sectionSeparator "What"
        , H.div [ A.class "form-label" ] [ H.text "Event name" ]
        , H.input [ A.attribute "data-testid" "event-editor-event-name", A.class "padded-input", A.value input.title, onInput (\t -> InternalMsg (UpdateEventInput picker { input | title = t })) ] []
        , case Photo.getPhotoUrl photoUploader of
            Just photoUrl ->
                H.div []
                    [ H.div
                        [ A.class "event-photo-wrapper" ]
                        [ H.img
                            [ A.src photoUrl
                            , A.class "event-photo"
                            ]
                            []
                        ]
                    , H.div [ A.class "button-wrapper" ]
                        [ H.button
                            [ onClick (InternalMsg (PhotoUploaderMsg Photo.clearPhoto))
                            , A.class "submit-button"
                            ]
                            [ H.text "Clear Photo" ]
                        ]
                    ]

            Nothing ->
                H.div [ A.class "button-wrapper" ]
                    [ H.button
                        [ onClick (InternalMsg (PhotoUploaderMsg Photo.addPhoto))
                        , A.class "submit-button"
                        ]
                        [ H.text "Add Photo" ]
                    ]
        , H.div [ A.class "form-label" ] [ H.text "Description" ]
        , expandingTextarea
            { text = input.description
            , onInput = \d -> InternalMsg (UpdateEventInput picker { input | description = d })
            , placeholder = ""
            }
        , sectionSeparator "When"
        , H.div [ A.class "icon-input-row date-picker-row", onClick (InternalMsg OpenPicker) ]
            [ iconInput Icon.calendar [ A.readonly True, A.value (viewEventDate timezone input.startTime) ]
            , iconInput Icon.clock [ A.readonly True, A.value (viewEventTime timezone input.startTime) ]
            ]
        , DP.view (DP.defaultSettings timezone (updatePicker input)) picker
        , sectionSeparator "Where"
        , H.div [ A.class "form-label" ] [ H.text "Location" ]
        , H.div [ A.class "icon-input-row" ]
            [ iconInput Icon.locationDot [ A.attribute "data-testid" "event-editor-event-location", A.value input.location, onInput (\l -> InternalMsg (UpdateEventInput picker { input | location = l })) ]
            ]
        , sectionSeparator (Maybe.withDefault "Password For Future Edits" <| Dict.get "password_header" copy)
        , H.div [ A.class "form-label" ] [ H.text "Password" ]
        , H.div [ A.class "icon-input-row" ]
            [ iconInput Icon.key [ A.attribute "data-testid" "event-editor-event-password", A.value input.password, onInput (\pw -> InternalMsg (UpdateEventInput picker { input | password = pw })) ]
            ]
        ]


{-| A text input with a decorative icon box glued to its left edge -}
iconInput : Icon.Icon Icon.WithoutId -> List (H.Attribute Msg) -> Html Msg
iconInput icon attributes =
    H.span [ A.class "icon-input" ]
        [ H.span [ A.class "icon-input-icon" ] [ Icon.view (Icon.styled [ Icon.lg ] icon) ]
        , H.input (A.class "padded-input" :: attributes) []
        ]


update : InternalMsg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateEventInput picker input ->
            ( { state | picker = picker, input = input }, Cmd.none )

        UpdateEventStartTime ( picker, mTime ) ->
            let
                newStartTime =
                    Maybe.withDefault state.input.startTime mTime

                oldInput =
                    state.input
            in
            ( { state | picker = picker, input = { oldInput | startTime = newStartTime } }, Cmd.none )

        OpenPicker ->
            let
                newPicker =
                    DP.openPicker
                        (pickerSettings state.timezone state.picker state.input)
                        state.input.startTime
                        (Just state.input.startTime)
                        state.picker
            in
            ( { state | picker = newPicker }, focusTimePickerOrTryAgainLater )

        FocusTimePicker ->
            ( state, focusTimePickerOrTryAgainLater )

        FocusTimePickerSoon ->
            ( state, delay100ms (InternalMsg FocusTimePicker) )

        PhotoUploaderMsg pumsg ->
            case pumsg of
                Photo.InternalMsg imsg ->
                    let
                        ( newPhotoState, photoMsg ) =
                            Photo.update imsg state.photoUploader
                    in
                    ( { state | photoUploader = newPhotoState }, Cmd.map (InternalMsg << PhotoUploaderMsg) photoMsg )

                Photo.PhotoUploadDone photoId ->
                    let
                        eventInput =
                            state.input
                    in
                    ( state, pureCmd (EventInputReady { eventInput | photoId = photoId }) )

                Photo.PhotoError errorMessage ->
                    ( { state | maybeModal = Just (PhotoUploadFailure errorMessage) }, Cmd.none )

        CloseModal ->
            ( { state | maybeModal = Nothing }, Cmd.none )

        DoNothing ->
            ( state, Cmd.none )


handleSubscription : State -> Sub Msg
handleSubscription { picker, input, timezone } =
    DP.subscriptions (pickerSettings timezone picker input) (InternalMsg << UpdateEventStartTime) picker


focusTimePickerOrTryAgainLater : Cmd Msg
focusTimePickerOrTryAgainLater =
    let
        handleFocusResult result =
            case result of
                Ok _ ->
                    InternalMsg DoNothing

                Err _ ->
                    InternalMsg FocusTimePickerSoon
    in
    Task.attempt handleFocusResult (Dom.focus "hour-select")


delay100ms : msg -> Cmd msg
delay100ms msg =
    Process.sleep 100 |> Task.perform (\_ -> msg)


pickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
pickerSettings timeZone picker input =
    let
        getValueFromPicker : ( DP.DatePicker, Maybe Time.Posix ) -> Msg
        getValueFromPicker ( dp, mTime ) =
            case mTime of
                Nothing ->
                    InternalMsg (UpdateEventInput dp input)

                Just newStart ->
                    InternalMsg (UpdateEventInput dp { input | startTime = newStart })
    in
    DP.defaultSettings timeZone getValueFromPicker


prepareInput : State -> ( State, Cmd Msg )
prepareInput state =
    let
        ( newPhotoState, photoCmd ) =
            Photo.submitPhoto state.photoUploader
    in
    ( { state | photoUploader = newPhotoState }, Cmd.map (InternalMsg << PhotoUploaderMsg) photoCmd )


pureCmd : msg -> Cmd msg
pureCmd =
    Task.perform identity << Task.succeed
