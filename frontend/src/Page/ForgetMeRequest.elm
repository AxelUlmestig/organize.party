module Page.ForgetMeRequest exposing (
    view,
    update
  )

import Types exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)

view : PageState ForgetMeRequestState -> Html ForgetMeRequestMsg
view pageState = H.text "forget me request"

update : ForgetMeRequestMsg -> PageState ForgetMeRequestState -> (PageState State, Cmd Msg)
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
      _ -> ( format (ForgetMeRequestState pageState.state), Cmd.none )
