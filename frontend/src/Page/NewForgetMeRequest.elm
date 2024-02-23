module Page.NewForgetMeRequest exposing (
    view,
    update
  )

import Types exposing (..)
import Html as H exposing (Html)
import Http exposing (Error(..))
import Regex
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)

view : PageState NewForgetMeRequestState -> Html NewForgetMeRequestMsg
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
            , onInput UpdateNewForgetMeRequestEmail
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
              , onClick (SubmitNewForgetMetRequest emailAddress)
              ]
              [ H.text "Submit" ]
            ]
        ]

    NewForgetMeRequestLoading ->
      H.div [ A.class "center" ]
        [ H.text "Loading..."
        ]

    NewForgetMeRequestSuccess email ->
          H.div []
            [ H.p
              [ A.class "about-paragraph" ]
              [ H.text ("An email has been sent to " ++ email ++ ". Please check your inbox and click the link to confirm the request.") ]
            ]


update : NewForgetMeRequestMsg -> PageState NewForgetMeRequestState -> (PageState State, Cmd Msg)
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
      UpdateNewForgetMeRequestEmail email -> ( format (NewForgetMeRequestState (NewForgetMeRequestInputtingEmail email)), Cmd.none )

      SubmitNewForgetMetRequest email -> ( format (NewForgetMeRequestState NewForgetMeRequestLoading), submitForgetMeRequest email )

      SubmittedNewForgetMetRequest httpResult ->
        case httpResult of
          Ok email ->
            ( format (NewForgetMeRequestState (NewForgetMeRequestSuccess email)), Cmd.none )
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
        , url = "/api/v1/forgetme/"
        , expect = Http.expectJson (NewForgetMeRequestMsg << SubmittedNewForgetMetRequest) newForgetMeRequestResponseDecoder
        , body = Http.jsonBody (encodeNewForgetMeRequest email)
        , timeout = Nothing
        , tracker = Nothing
        }

