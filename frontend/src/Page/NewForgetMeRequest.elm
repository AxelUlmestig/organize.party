module Page.NewForgetMeRequest exposing
    ( Msg(..)
    , State
    , init
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import Regex
import Shared.PageState exposing (PageState, mapPageState)
import Types exposing (..)


type State
    = InputtingEmail String
    | Success String
    | Loading
    | Failure


type Msg
    = InternalMsg InternalMsg


type InternalMsg
    = UpdateEmail String
    | SubmitRequest String
    | VerifyResult (Result Http.Error String)


init : ( State, Cmd Msg )
init =
    ( InputtingEmail "", Cmd.none )


view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        InputtingEmail emailAddress ->
            H.div []
                [ H.p [ A.class "about-paragraph" ] [ H.text "Enter the email that you want to be purged from the database. An email with a confirmation link will be sent to you." ]
                , H.div []
                    [ H.input
                        [ A.class "padded-input"
                        , A.class "rounded-corners"
                        , A.style "width" "100%"
                        , A.attribute "type" "email"
                        , A.attribute "autocomplete" "email"
                        , A.value emailAddress
                        , onInput (InternalMsg << UpdateEmail)
                        , A.placeholder "Your email"
                        ]
                        []
                    ]
                , H.div [ A.class "button-wrapper" ]
                    [ H.button
                        [ A.class "submit-button"
                        , disableUnlessValidEmail emailAddress
                        , onClick (InternalMsg <| SubmitRequest emailAddress)
                        ]
                        [ H.text "Submit" ]
                    ]
                ]

        Success email ->
            H.div []
                [ H.p
                    [ A.class "about-paragraph" ]
                    [ H.text ("An email has been sent to " ++ email ++ ". Please check your inbox and click the link to confirm the request.") ]
                ]

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
        UpdateEmail email ->
            ( format (InputtingEmail email), Cmd.none )

        SubmitRequest email ->
            ( format Loading, submitForgetMeRequest email )

        VerifyResult httpResult ->
            case httpResult of
                Ok email ->
                    ( format (Success email), Cmd.none )

                Err _ ->
                    ( format Failure, Cmd.none )


disableUnlessValidEmail email =
    let
        validEmailRegex =
            "^ *[a-zA-Z0-9.!#$%&''*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)* *$"

        mRegex =
            Regex.fromString validEmailRegex

        mContains =
            Maybe.map (\regex -> Regex.contains regex email) mRegex
    in
    A.disabled (mContains /= Just True)


submitForgetMeRequest : String -> Cmd Msg
submitForgetMeRequest email =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/forget-me/"
        , expect = Http.expectJson (InternalMsg << VerifyResult) newForgetMeRequestResponseDecoder
        , body = Http.jsonBody (encodeNewForgetMeRequest email)
        , timeout = Nothing
        , tracker = Nothing
        }


encodeNewForgetMeRequest : String -> Value
encodeNewForgetMeRequest email =
    Encode.object
        [ ( "email", Encode.string email ) ]


newForgetMeRequestResponseDecoder : D.Decoder String
newForgetMeRequestResponseDecoder =
    D.field "email" D.string
