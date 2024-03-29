module Shared.ViewAttendees exposing (viewAttendees)

import Dict exposing (Dict)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Shared.FormatUrls exposing (formatTextWithLinks)
import Types exposing (..)


viewAttendees : List Attendee -> Html msg
viewAttendees attendees =
    let
        attendeeDict =
            splitAttendees attendees
    in
    H.div []
        [ case Dict.get "Coming" attendeeDict of
            Nothing ->
                H.h3 [ A.attribute "data-testid" "view-attendees-attending-number" ] [ H.text "Attending: 0" ]

            Just attending ->
                let
                    ( comingCount, plusOnesCount ) =
                        countAttendees attending
                in
                H.div []
                    [ H.h3 [ A.attribute "data-testid" "view-attendees-attending-number" ]
                        [ H.text
                            ("Attending: "
                                ++ String.fromInt comingCount
                                ++ (if plusOnesCount == 0 then
                                        ""

                                    else
                                        " (+" ++ String.fromInt plusOnesCount ++ ")"
                                   )
                            )
                        ]
                    , H.div []
                        (List.map
                            (\attendee ->
                                H.div []
                                    [ H.text
                                        (attendee.name
                                            ++ (if attendee.plusOne then
                                                    " (+1)"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                            )
                            attending
                        )
                    ]
        , H.br [] []
        , case Dict.get "Maybe Coming" attendeeDict of
            Nothing ->
                H.div [] []

            Just maybeAttending ->
                let
                    ( maybeComingCount, plusOnesCount ) =
                        countAttendees maybeAttending
                in
                H.div []
                    [ H.h3 [ A.attribute "data-testid" "view-attendees-maybe-attending-number" ]
                        [ H.text
                            ("Maybe Attending: "
                                ++ String.fromInt maybeComingCount
                                ++ (if plusOnesCount == 0 then
                                        ""

                                    else
                                        " (+" ++ String.fromInt plusOnesCount ++ ")"
                                   )
                            )
                        ]
                    , H.div []
                        (List.map
                            (\attendee ->
                                H.div []
                                    [ H.text
                                        (attendee.name
                                            ++ (if attendee.plusOne then
                                                    " (+1)"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                            )
                            maybeAttending
                        )
                    ]
        , H.br [] []
        , case Dict.get "Not Coming" attendeeDict of
            Nothing ->
                H.div [] []

            Just notAttending ->
                let
                    ( notComingCount, plusOnesCount ) =
                        countAttendees notAttending
                in
                H.div []
                    [ H.h3 [] [ H.text ("Can't Attend: " ++ String.fromInt notComingCount) ]
                    , H.div [] (List.map (\attendee -> H.div [] [ H.text attendee.name ]) notAttending)
                    ]
        ]


splitAttendees : List Attendee -> Dict String (List Attendee)
splitAttendees =
    listToDict (attendeeStatusToString << .status)


countAttendees : List Attendee -> ( Int, Int )
countAttendees =
    List.foldl
        (\attendee ( coming, plusOne ) ->
            ( coming + 1
            , plusOne
                + (if attendee.plusOne then
                    1

                   else
                    0
                  )
            )
        )
        ( 0, 0 )


listToDict : (a -> comparable) -> List a -> Dict comparable (List a)
listToDict getKey =
    let
        updateExisting newValue maybeExisting =
            case maybeExisting of
                Nothing ->
                    Just [ newValue ]

                Just list ->
                    Just (newValue :: list)

        f x =
            Dict.update (getKey x) (updateExisting x)
    in
    List.foldr f Dict.empty
