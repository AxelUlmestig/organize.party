module Page.NewEvent exposing (handleSubscription, update, view)

import Browser
import Browser.Navigation as Nav
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601 as Iso8601
import Shared.SectionSeparator exposing (sectionSeparator)
import SingleDatePicker as DP
import Time as Time
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)
import Json.Encode as Json
import TimePicker exposing (TimeEvent(..), TimePicker)

borderRadius =
    A.style "border-radius" "5px"


view : PageState NewEventState -> Html Msg
view pageState =
    case pageState.state of
        NewEventLoading ->
            H.div [ A.class "center" ]
              [ H.text "Loading..."
              ]

        NewEvent { datePicker, timePicker, input } ->
            let
                updatePicker : EventInput -> ( DP.DatePicker, Maybe Time.Posix ) -> Msg
                updatePicker input2 ( datePicker2, mTimestamp ) =
                    case mTimestamp of
                        Just newStart ->
                            NewEventMsg (UpdateEventInput datePicker2 timePicker { input2 | startTime = newStart })

                        Nothing ->
                            NewEventMsg (UpdateEventInput datePicker2 timePicker input2)
            in
            H.div []
                [ H.h1 [ A.class "mb-3" ] [ H.text "Create an event" ]
                , sectionSeparator "What"
                , H.div [] [ H.text "Event name" ]
                , H.div []
                  [ H.input
                    [ A.style "width" "100%"
                    , borderRadius
                    , A.value input.title
                    , onInput (\t -> NewEventMsg (UpdateEventInput datePicker timePicker { input | title = t }))
                    ] []
                  ]
                , H.div [] [ H.text "Description" ]
                , H.div []
                  [ H.textarea
                    [ A.style "width" "100%"
                    , borderRadius
                    , A.value input.description
                    , onInput (\d -> NewEventMsg (UpdateEventInput datePicker timePicker { input | description = d }))
                    ] []
                  ]
                , sectionSeparator "When"
                , H.input [ A.property "type" (Json.string "time"), A.name "time" ] []
                , H.div [ A.style "display" "flex", A.style "color" "black", onClick (NewEventMsg OpenPicker) ]
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
                , DP.view (DP.defaultSettings pageState.timeZone (updatePicker input)) datePicker
                , sectionSeparator "Where"
                , H.div [] [ H.text "Location" ]
                , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
                    [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                        [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.locationDot) ]
                    , H.input
                      [ A.style "width" "100%"
                      , A.style "border-radius" "0 5px 5px 0"
                      , A.value input.location
                      , onInput (\l -> NewEventMsg (UpdateEventInput datePicker timePicker { input | location = l }))
                      ] []
                    ]
                , sectionSeparator "Password For Future Edits"
                , H.div [] [ H.text "Password" ]
                , H.div [ A.class "d-flex flex-row justify-content-start", A.style "margin-top" "1rem" ]
                    [ H.span [ A.style "background-color" "#eaebef", A.style "width" "2rem", A.style "height" "2rem", A.style "display" "flex", A.style "align-items" "center", A.style "border-radius" "5px 0 0 5px" ]
                        [ Icon.view (Icon.styled [ Icon.lg, A.style "display" "block", A.style "margin" "auto" ] Icon.key) ]
                    , H.input
                      [ A.style "width" "100%"
                      , A.style "border-radius" "0 5px 5px 0"
                      , A.value input.password
                      , onInput (\pw -> NewEventMsg (UpdateEventInput datePicker timePicker { input | password = pw }))
                      ] []
                    ]
                , H.div [ A.class "text-center", A.style "margin-top" "1rem" ]
                    [ H.button [ A.style "background-color" "#1c2c3b", onClick (NewEventMsg (CreateEventMsg input)), A.class "btn btn-primary" ] [ H.text "Submit" ]
                    ]
                ]


update : NewEventMsg -> PageState NewEventState -> ( PageState State, Cmd Msg )
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState
    in
    case msg of
        UpdateEventInput datePicker timePicker input ->
            let
                newPageState =
                    { state = NewEventState (NewEvent { datePicker = datePicker, timePicker = timePicker, input = input })
                    , timeZone = pageState.timeZone
                    , key = pageState.key
                    , pageUrl = pageState.pageUrl
                    }
            in
            ( newPageState, Cmd.none )

        UpdateEventStartDate ( datePicker, mTime ) ->
            case pageState.state of
                NewEvent oldState ->
                    let
                        newStartTime =
                            Maybe.withDefault oldState.input.startTime mTime

                        oldInput =
                            oldState.input

                        newEventState =
                            NewEventState (NewEvent { datePicker = datePicker, input = { oldInput | startTime = newStartTime } })

                        newPageState =
                            { state = newEventState
                            , timeZone = pageState.timeZone
                            , key = pageState.key
                            , pageUrl = pageState.pageUrl
                            }
                    in
                    ( newPageState, Cmd.none )

                _ ->
                    ( format (NewEventState pageState.state), Cmd.none )

        UpdateEventStartTime m ->
            case pageState.state of
                NewEvent oldState ->
                    -- TODO
                    ( format (NewEventState pageState.state), Cmd.none )
                _ ->
                    ( format (NewEventState pageState.state), Cmd.none )

        OpenPicker ->
            case pageState.state of
                NewEvent oldState ->
                    let
                        newPicker =
                            DP.openPicker
                                (datePickerSettings pageState.timeZone oldState.datePicker oldState.input)
                                oldState.input.startTime
                                (Just oldState.input.startTime)
                                oldState.datePicker

                        newPageState =
                            { state = NewEventState (NewEvent { oldState | datePicker = newPicker })
                            , timeZone = pageState.timeZone
                            , key = pageState.key
                            , pageUrl = pageState.pageUrl
                            }
                    in
                    ( newPageState, Cmd.none )

                _ ->
                    ( format (NewEventState pageState.state), Cmd.none )

        CreateEventMsg input ->
            ( format (NewEventState NewEventLoading), createNewEvent input )

        CreatedEvent result ->
            case result of
                Ok event ->
                    ( format (ViewEventState (ViewEvent (Just InviteGuestsInfoModal) event (emptyAttendeeInput event.id))), Nav.pushUrl pageState.key ("/e/" ++ event.id) )

                Err _ ->
                    ( format Failure, Cmd.none )


datePickerSettings : Time.Zone -> DP.DatePicker -> EventInput -> DP.Settings Msg
datePickerSettings timeZone datePicker input =
    let
        getValueFromPicker : ( DP.DatePicker, Maybe Time.Posix ) -> Msg
        getValueFromPicker ( dp, mTime ) =
            case mTime of
                Nothing ->
                    NewEventMsg (UpdateEventInput dp input)

                Just newStart ->
                    NewEventMsg (UpdateEventInput dp { input | startTime = newStart })
    in
    DP.defaultSettings timeZone getValueFromPicker


timePickerSettings : TimePicker.Settings
timePickerSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
    { defaultSettings | showSeconds = False, minuteStep = 1, use24Hours = True }



createNewEvent : EventInput -> Cmd Msg
createNewEvent input =
    Http.post
        { url = "/api/v1/events"
        , expect = Http.expectJson (NewEventMsg << CreatedEvent) eventDecoder
        , body = Http.jsonBody (encodeEventInput input)
        }


handleSubscription : PageState NewEventState -> Sub Msg
handleSubscription pageState =
    case pageState.state of
        NewEvent { datePicker, timePicker, input } -> DP.subscriptions (datePickerSettings pageState.timeZone datePicker input) (NewEventMsg << UpdateEventStartDate timePicker) datePicker

        _ ->
            Sub.none
