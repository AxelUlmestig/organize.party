module Page.NewForgetMeRequest exposing (
    view,
    update,
    init,
    NewForgetMeRequestState,
    NewForgetMeRequestMsg(..)
  )

import Types exposing (..)
import Json.Encode as Encode exposing (Value)
import Html as H exposing (Html)
import Http exposing (Error(..))
import Regex
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)

type NewForgetMeRequestState
    = NewForgetMeRequestInputtingEmail String
    | NewForgetMeRequestSuccess String
    | Loading
    | Failure

type NewForgetMeRequestMsg
    = NewForgetMeRequestInternalMsg NewForgetMeRequestInternalMsg

type NewForgetMeRequestInternalMsg
    = UpdateEmail String
    | SubmitRequest String
    | SubmittedRequest (Result Http.Error String)

init : ( NewForgetMeRequestState, Cmd NewForgetMeRequestMsg )
init = ( NewForgetMeRequestInputtingEmail "", Cmd.none )

view : PageState navbarState NewForgetMeRequestState -> Html NewForgetMeRequestMsg
view pageState =
  case pageState.state of
    NewForgetMeRequestInputtingEmail emailAddress ->
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
            , onInput (NewForgetMeRequestInternalMsg << UpdateEmail)
            , A.placeholder "Your email"
            ] []
          ]

        , H.div [ A.class "text-center", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
            [ H.button
              [ A.style "background-color" "#1c2c3b"
              , A.class "btn btn-primary"
              , A.style "margin-top" "1rem"
              , A.style "margin-bottom" "1rem"
              , disableUnlessValidEmail emailAddress
              , onClick (NewForgetMeRequestInternalMsg <| SubmitRequest emailAddress)
              ]
              [ H.text "Submit" ]
            ]
        ]

    NewForgetMeRequestSuccess email ->
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


update : NewForgetMeRequestInternalMsg -> PageState navbarState NewForgetMeRequestState -> (PageState navbarState NewForgetMeRequestState, Cmd NewForgetMeRequestMsg)
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
      UpdateEmail email -> ( format (NewForgetMeRequestInputtingEmail email), Cmd.none )

      SubmitRequest email -> ( format Loading, submitForgetMeRequest email )

      SubmittedRequest httpResult ->
        case httpResult of
          Ok email ->
            ( format (NewForgetMeRequestSuccess email), Cmd.none )
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

submitForgetMeRequest : String -> Cmd NewForgetMeRequestMsg
submitForgetMeRequest email =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/v1/forget-me/"
        , expect = Http.expectJson (NewForgetMeRequestInternalMsg << SubmittedRequest) newForgetMeRequestResponseDecoder
        , body = Http.jsonBody (encodeNewForgetMeRequest email)
        , timeout = Nothing
        , tracker = Nothing
        }

encodeNewForgetMeRequest : String -> Value
encodeNewForgetMeRequest email =
    Encode.object
        [ ( "email", Encode.string email ) ]
