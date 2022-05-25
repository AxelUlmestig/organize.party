module Util exposing (viewEventDate)

import Date
import Iso8601
import Time
import Time.Extra exposing (toOffset, Interval(..), add)
import Html as H exposing (Html)

import Types exposing (..)

viewEventDate : Time.Zone -> Time.Posix -> Time.Posix -> Html Msg
viewEventDate timeZone start end =
    let
        oneDayMillis = 24 * 60 * 60 * 1000
        timeDiff = Time.posixToMillis end - Time.posixToMillis start

        formatTime : Time.Posix -> String
        formatTime time = String.padLeft 2 '0' (String.fromInt (Time.toHour timeZone time)) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute timeZone time))

        formatDate : Time.Posix -> String
        formatDate time = Date.toIsoString (Date.fromPosix timeZone time)
    in if timeDiff < oneDayMillis
    then H.div [] [ H.text (formatDate start ++ ", " ++ formatTime start ++ " - " ++ formatTime end) ]
    else H.div [] [ H.text (formatDate start ++ " " ++ formatTime start ++ ", " ++ formatDate end ++ " " ++ formatTime end) ]

