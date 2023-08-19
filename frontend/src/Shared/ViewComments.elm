module Shared.ViewComments exposing (viewComments)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Types exposing (..)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Shared.FormatUrls exposing (formatTextWithLinks)
import Time


viewComments : List Comment -> Html msg
viewComments comments =
  H.div []
    <| List.map viewComment
    <| List.sortBy (((*) (-1)) << Time.posixToMillis << .timestamp) comments

viewComment : Comment -> Html msg
viewComment comment =
  H.div []
    [ H.text comment.name
    , H.div [ A.class "comment" ]
      [ formatTextWithLinks comment.comment
      ]
    ]

