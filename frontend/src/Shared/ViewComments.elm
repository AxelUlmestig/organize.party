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
import Util
import Time
import String

viewComments : Time.Posix -> List Comment -> Html msg
viewComments currentTime comments =
  H.div []
    <| List.map (viewComment currentTime)
    <| List.sortBy (((*) (-1)) << Time.posixToMillis << .timestamp) comments

viewComment : Time.Posix -> Comment -> Html msg
viewComment currentTime comment =
  H.div [ A.class "comment" ]
    [ H.text comment.name
    , H.span
      [ A.class "comment-timestamp" ]
      [ H.text (formatTimestamp currentTime comment.timestamp) ]
    , H.div [ A.class "comment-bubble" ]
      [ formatTextWithLinks comment.comment
      ]
    ]

formatTimestamp : Time.Posix -> Time.Posix -> String
formatTimestamp currentTime timestamp =
  let seconds = (Time.posixToMillis currentTime - Time.posixToMillis timestamp) // 1000
      minutes = seconds // 60
      hours = minutes // 60
      days = hours // 24
      months = days // 31
      years = days // 365

      timeSinceComment =
        Maybe.withDefault "just now"
          << List.head
          << List.map Tuple.second
          <| List.filter (((<)0) << Tuple.first)
            [ (years, String.fromInt years ++ " years ago")
            , (months, String.fromInt months ++ " months ago")
            , (days, String.fromInt days ++ " days ago")
            , (hours, String.fromInt hours ++ " hours ago")
            , (minutes, String.fromInt minutes ++ " minutes ago")
            , (seconds, String.fromInt seconds ++ " seconds ago")
            ]
  in " - " ++ timeSinceComment
