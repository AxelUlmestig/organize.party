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
    [ H.img
      [ A.class "profile-picture"
      , A.src (comment.gravatarUrl ++ "?s=80&d=404")
      , A.alt ""
      , A.title "Upload an image at https://gravatar.com to get a profile picture"
      ]
      []
    , H.span [ A.class "comment-content" ]
      [ H.text comment.name
      , H.span
        [ A.class "comment-timestamp" ]
        [ H.text (formatTimestamp currentTime comment.timestamp) ]
      , H.div [ A.class "comment-bubble" ]
        [ formatTextWithLinks comment.comment
        ]
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

      formatTime time unit =
        if time == 1 then
          "1 " ++ unit ++ " ago"
        else
          String.fromInt time ++ " " ++ unit ++ "s ago"

      timeSinceComment =
        Maybe.withDefault "just now"
          << List.head
          << List.map Tuple.second
          <| List.filter (((<)0) << Tuple.first)
            [ (years, formatTime years "year")
            , (months, formatTime months "month")
            , (days, formatTime days "day")
            , (hours, formatTime hours "hour")
            , (minutes, formatTime minutes "minute")
            , (seconds, formatTime seconds "second")
            ]
  in " - " ++ timeSinceComment
