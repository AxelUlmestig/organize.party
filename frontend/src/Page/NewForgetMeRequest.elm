module Page.NewForgetMeRequest exposing (
    view,
    update
  )

import Types exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)

view : PageState NewForgetMeRequestState -> Html NewForgetMeRequestMsg
view pageState = H.text "new forget me request"

update : NewForgetMeRequestMsg -> PageState NewForgetMeRequestState -> (PageState State, Cmd Msg)
update msg pageState =
    let
        format =
            \x -> mapPageState (always x) pageState

        { key, timeZone, state } =
            pageState
    in
    case msg of
      _ -> ( format (NewForgetMeRequestState pageState.state), Cmd.none )
