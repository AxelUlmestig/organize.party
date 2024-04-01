module Page.Unsubscribe exposing
    ( Msg(..)
    , State
    , handleSubscription
    , init
    , update
    , view
    )

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
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
import Json.Decode as D
import Platform.Sub as Sub
import Process
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Shared.PageState exposing (PageState, setPageState)
import Shared.SectionSeparator exposing (sectionSeparator)
import Shared.Void exposing (Void, absurd)
import SingleDatePicker as DP
import Task
import Time
import Types exposing (Event, eventDecoder)
import Util exposing (viewEventDate, viewEventTime)


type State
    = Loading
    | Unsubscribed UnsubscribeResult
    | NotFound
    | Failure


type alias UnsubscribeResult =
    { email : Maybe String
    , unsubscribedAt : Time.Posix
    , event : Event
    }


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = VerifyResult (Result Http.Error UnsubscribeResult)


init : String -> ( State, Cmd Msg )
init unsubscribeId =
    let
        sendRequest =
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/v1/unsubscribe/" ++ unsubscribeId
                , expect = Http.expectJson (InternalMsg << VerifyResult) decoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    ( Loading, sendRequest )


view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        Loading ->
            H.div [ A.class "center" ]
                [ H.text "Loading..."
                ]

        Failure ->
            H.div [ A.class "center" ]
                [ H.text "Something went wrong, please try again later"
                ]

        NotFound ->
            H.div [ A.class "center" ]
                [ H.text "Error: There's no attendee attendee and event with the unsubscribe id in the URL"
                ]

        Unsubscribed unsubscribeResult ->
            case unsubscribeResult.email of
                Nothing ->
                    H.div []
                        [ H.p
                            [ A.class "about-paragraph" ]
                            [ H.text "Your email has already been deleted from our database. You won't receive any more emails from this event."
                            ]
                        ]

                Just email ->
                    H.div []
                        [ H.p
                            [ A.class "about-paragraph" ]
                            [ H.b [] [ H.text email ]
                            , H.text " Will never receive an email update from "
                            , H.a [ A.href ("/e/" ++ unsubscribeResult.event.id) ] [ H.text unsubscribeResult.event.title ]
                            , H.text " ever again."
                            ]
                        , H.p
                            [ A.class "about-paragraph" ]
                            [ H.text "But you can still receive updates from other events that you have interacted with. If you want to completely purge your email address from our database you can click "
                            , H.a [ A.href "/forget-me" ] [ H.text "here" ]
                            , H.text "."
                            ]
                        ]


update : InternalMsg -> PageState navbarState State -> ( PageState navbarState State, Cmd Msg )
update msg pageState =
    case msg of
        VerifyResult httpResult ->
            case httpResult of
                Ok unsubscribeResult ->
                    ( setPageState (Unsubscribed unsubscribeResult) pageState, Cmd.none )

                Err (BadStatus 404) ->
                    ( setPageState NotFound pageState, Cmd.none )

                Err _ ->
                    ( setPageState Failure pageState, Cmd.none )


handleSubscription : PageState navbarState State -> Sub Msg
handleSubscription pageState =
    Sub.none


decoder : D.Decoder UnsubscribeResult
decoder =
    D.map3 UnsubscribeResult
        (D.maybe (D.field "email" D.string))
        (D.field "unsubscribedAt" Iso8601.decoder)
        (D.field "event" eventDecoder)
