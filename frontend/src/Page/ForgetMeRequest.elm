module Page.ForgetMeRequest exposing (
    view,
    update,
    init,
    ForgetMeRequestState,
    ForgetMeRequestMsg(..)
  )

import Types exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Maybe
import Http
import Util exposing (viewEventDate, viewEventTime)
import Time as Time

type alias ForgetMeRequest =
    { id : String
    , email : Maybe String
    , deletedAt : Maybe Time.Posix
    }

type ForgetMeRequestState
    = Loading
    | Failure
    | ViewForgetMeRequest ForgetMeRequest

type ForgetMeRequestMsg
    = ForgetMeRequestInternalMsg ForgetMeRequestInternalMsg

type ForgetMeRequestInternalMsg
    = LoadedForgetMeRequest (Result Http.Error ForgetMeRequest)
    | SubmitForgetMeRequest String
    | SubmittedForgetMeRequest Time.Posix

init : String -> ( ForgetMeRequestState, Cmd ForgetMeRequestMsg )
init forgetMeRequestId = 
    let cmd = 
          Http.get
              { url = "/api/v1/forget-me/" ++ forgetMeRequestId
              , expect = Http.expectJson (ForgetMeRequestInternalMsg << LoadedForgetMeRequest) forgetMeRequestDecoder
              }
    in ( Loading, cmd )

view : PageState navbarState ForgetMeRequestState -> Html ForgetMeRequestMsg
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
                [ onClick (ForgetMeRequestInternalMsg <| SubmitForgetMeRequest id)
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

    Loading ->
      H.div [ A.class "center" ]
        [ H.text "Loading..."
        ]

    Failure ->
      H.div [ A.class "center" ]
        [ H.text "Something went wrong, please try again later"
        ]

update : ForgetMeRequestInternalMsg -> PageState navbarState ForgetMeRequestState -> (PageState navbarState ForgetMeRequestState, Cmd ForgetMeRequestMsg)
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
            ( format (ViewForgetMeRequest forgetMeRequest), Cmd.none )
          Err _ ->
            ( format Failure, Cmd.none )
      SubmitForgetMeRequest forgetMeRequestId ->
        let httpRequest =
              Http.request
                  { method = "DELETE"
                  , url = "/api/v1/forget-me/" ++ forgetMeRequestId
                  , headers = []
                  , body = Http.emptyBody
                  , expect = Http.expectJson (ForgetMeRequestInternalMsg << LoadedForgetMeRequest) forgetMeRequestDecoder
                  , timeout = Nothing
                  , tracker = Nothing
                  }
        in (format Loading, httpRequest )
      _ -> ( pageState, Cmd.none )

