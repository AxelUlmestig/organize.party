module Page.ForgetMeRequest exposing
    ( Msg(..)
    , State
    , init
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601
import Json.Decode as D
import Maybe
import Shared.PageState exposing (PageState, mapPageState)
import Time
import Types exposing (..)
import Util exposing (viewEventDate, viewEventTime)


type alias ForgetMeRequest =
    { id : String
    , email : Maybe String
    , deletedAt : Maybe Time.Posix
    }


type State
    = Loading
    | Failure
    | View ForgetMeRequest


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = Loaded (Result Http.Error ForgetMeRequest)
    | Submit String
    | Submitted Time.Posix


init : String -> ( State, Cmd Msg )
init forgetMeRequestId =
    let
        cmd =
            Http.get
                { url = "/api/v1/forget-me/" ++ forgetMeRequestId
                , expect = Http.expectJson (InternalMsg << Loaded) forgetMeRequestDecoder
                }
    in
    ( Loading, cmd )


view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        View { id, email, deletedAt } ->
            case ( email, deletedAt ) of
                ( Just emailValue, _ ) ->
                    H.div [ A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
                        [ H.text "Are you sure that you want to purge all references to "
                        , H.b [] [ H.text emailValue ]
                        , H.text " from the database?"
                        , H.div [ A.class "button-wrapper" ]
                            [ H.button
                                [ onClick (InternalMsg <| Submit id)
                                , A.style "background-color" "#1c2c3b"
                                , A.class "submit-button"
                                ]
                                [ H.text "Yes, forget me" ]
                            ]
                        ]

                ( _, Just deletedAtValue ) ->
                    H.div [ A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
                        [ H.text "The request to forget you was successfully processed on "
                        , H.b []
                            [ H.text (viewEventDate pageState.timeZone deletedAtValue)
                            , H.text " "
                            , H.text (viewEventTime pageState.timeZone deletedAtValue)
                            ]
                        , H.text ". Any data you have provided since then is still in the database."
                        ]

                _ ->
                    H.div [] []

        Loading ->
            H.div [ A.class "center" ]
                [ H.text "Loading..."
                ]

        Failure ->
            H.div [ A.class "center" ]
                [ H.text "Something went wrong, please try again later"
                ]


update : InternalMsg -> PageState navbarState State -> ( PageState navbarState State, Cmd Msg )
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
        Loaded result ->
            case result of
                Ok forgetMeRequest ->
                    ( format (View forgetMeRequest), Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )

        Submit forgetMeRequestId ->
            let
                httpRequest =
                    Http.request
                        { method = "DELETE"
                        , url = "/api/v1/forget-me/" ++ forgetMeRequestId
                        , headers = []
                        , body = Http.emptyBody
                        , expect = Http.expectJson (InternalMsg << Loaded) forgetMeRequestDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
            in
            ( format Loading, httpRequest )

        _ ->
            ( pageState, Cmd.none )


forgetMeRequestDecoder : D.Decoder ForgetMeRequest
forgetMeRequestDecoder =
    D.map3 ForgetMeRequest
        (D.field "id" D.string)
        (D.maybe <| D.field "email" D.string)
        (D.maybe <| D.field "deletedAt" Iso8601.decoder)
