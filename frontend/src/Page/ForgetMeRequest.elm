module Page.ForgetMeRequest exposing (
    view,
    update,
    fetchForgetMeRequest
  )

import Types exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Maybe
import Http
import Util exposing (viewEventDate, viewEventTime)

view : PageState ForgetMeRequestState -> Html ForgetMeRequestMsg
view pageState =
  case pageState.state of
    ViewForgetMeRequest { id, email, deletedAt } ->
      case (email, deletedAt) of
        (Just emailValue, _) ->
          H.div [ A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
            [ H.text "Are you sure that you want to purge all references to "
            , H.b [] [ H.text emailValue ]
            , H.text " from the database?"
            , H.div [ A.class "text-center", A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
              [ H.button
                [ onClick (SubmitForgetMeRequest id)
                , A.style "background-color" "#1c2c3b"
                , A.class "btn btn-primary"
                ]
                [ H.text "Yes, forget me" ]
              ]
            ]
        (_, Just deletedAtValue) ->
          H.div [ A.style "margin-top" "1rem", A.style "margin-bottom" "1rem" ]
            [ H.text "The request to forget you was successfully processed on "
            , H.b []
              [ H.text (viewEventDate pageState.timeZone deletedAtValue)
              , H.text " "
              , H.text (viewEventTime pageState.timeZone deletedAtValue)
              ]
            , H.text ". Any data you have provided since then is still in the database."
            ]
        _ -> H.div [] []
    ForgetMeRequestLoading ->
      H.div [ A.class "center" ]
        [ H.text "Loading..."
        ]

update : ForgetMeRequestMsg -> PageState ForgetMeRequestState -> (PageState State, Cmd Msg)
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
      LoadedForgetMeRequest result ->
        case result of
          Ok forgetMeRequest ->
            ( format (ForgetMeRequestState (ViewForgetMeRequest forgetMeRequest)), Cmd.none )
          Err _ ->
            ( format Failure, Cmd.none )
      SubmitForgetMeRequest forgetMeRequestId ->
        let httpRequest =
              Http.request
                  { method = "DELETE"
                  , url = "/api/v1/forget-me/" ++ forgetMeRequestId
                  , headers = []
                  , body = Http.emptyBody
                  , expect = Http.expectJson (ForgetMeRequestMsg << LoadedForgetMeRequest) forgetMeRequestDecoder
                  , timeout = Nothing
                  , tracker = Nothing
                  }
        in (format (ForgetMeRequestState ForgetMeRequestLoading), httpRequest )
      _ -> ( format (ForgetMeRequestState pageState.state), Cmd.none )

fetchForgetMeRequest : String -> Cmd Msg
fetchForgetMeRequest id =
    Http.get
        { url = "/api/v1/forget-me/" ++ id
        , expect = Http.expectJson (ForgetMeRequestMsg << LoadedForgetMeRequest) forgetMeRequestDecoder
        }

