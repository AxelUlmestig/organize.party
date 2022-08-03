module Page.EditEvent exposing (view, update, fetchEvent)

import Browser
import Html as H exposing (Html)
import Http

import Types exposing (..)

view : PageState EditEventState -> Html Msg
view _ = H.text "TODO"

update : EditEventMsg -> PageState EditEventState -> ( PageState State, Cmd Msg )
update _ pageState =
  let
    format = \x -> mapPageState (always x) pageState
  in ( format (EditEventState pageState.state), Cmd.none )

fetchEvent : String -> Cmd Msg
fetchEvent id =
    Http.get
        { url = "/api/v1/events/" ++ id
        , expect = Http.expectJson (EditEventMsg << LoadedEventForEdit) eventDecoder
        }

