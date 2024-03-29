module Page.About exposing (
    handleSubscription,
    update,
    view,
    init,
    Msg(..),
    State
  )

import Browser
import Browser.Navigation as Nav
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Iso8601 as Iso8601
import Shared.SectionSeparator exposing (sectionSeparator)
import SingleDatePicker as DP
import Time as Time
import Shared.PageState exposing (PageState)
import Util exposing (viewEventDate, viewEventTime)
import Browser.Dom as Dom
import Task
import Process
import Shared.ExpandingTextarea exposing (expandingTextarea)
import Platform.Sub as Sub
import Dict exposing (Dict)
import Shared.Void exposing (Void, absurd)

borderRadius =
    A.style "border-radius" "5px"

init : ( State, Cmd Msg )
init = ( (), Cmd.none )

view : PageState navbarState State -> Html Msg
view pageState =
    case pageState.state of
        () ->
            H.div []
              [ H.p [ A.class "about-paragraph"]
                [ H.a [ A.href "/" ] [ H.text "organize.party"]
                , H.text " is a free to use, ad free, tracker free, "
                , H.a [ A.href "https://github.com/AxelUlmestig/organize.party" ] [ H.text "open source"]
                , H.text " event planning tool."
                ]
              , sectionSeparator "How it works"
              , H.p [ A.class "about-paragraph"]
                [ H.ol []
                  [ H.li [] [ H.text "Create an event, describing when and where it will take place." ]
                  , H.li [] [ H.text "When the event is created you can share the page with your friends and they'll be able to RSVP." ]
                  , H.li [] [ H.text "Once your friends have RSVP'd they'll get an email that will insert the event into their phone's calendar with a link to the event." ]
                  , H.li [] [ H.text "It's possible to edit the event by clicking the pen icon in the top right corner. To be able to edit the event you need to submit the same password that you submitted when creating the event." ]
                  , H.li [] [ H.text "An automatic email will be sent to everyone who's RSVP'd, updating the event in their calendars." ]
                  , H.li [] [ H.text "RSVP status can be updated by RSVP'ing again with the same email address. This will overwrite your previous RSVP" ]
                  ]
                ]
              ]



update : InternalMsg -> PageState navbarState State -> ( PageState navbarState State, Cmd Msg )
update = absurd

handleSubscription : PageState navbarState State -> Sub Msg
handleSubscription pageState = Sub.none

type Msg
  = InternalMsg InternalMsg

type alias InternalMsg = Void
type alias State = ()
