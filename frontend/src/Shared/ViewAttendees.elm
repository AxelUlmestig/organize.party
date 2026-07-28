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
import Maybe
import Shared.FormatUrls exposing (formatTextWithLinks)
import Types exposing (..)


viewAttendees : List Attendee -> Html msg
viewAttendees attendees =
    let
        attendeeDict =
            splitAttendees attendees

        attendingHtml =
            let
                options =
                    { testId = "view-attendees-attending-number"
                    , attendeeCategory = "COMING"
                    , displayPlusOne = True
                    }
            in
            Dict.get (attendeeStatusToString Coming) attendeeDict
                |> Maybe.map (displayAttendees options)
                |> Maybe.withDefault (H.h3 [ A.attribute "data-testid" "view-attendees-attending-number" ] [ H.text "Attending: 0" ])

        maybeAttendingHtml =
            let
                options =
                    { testId = "view-attendees-maybe-attending-number"
                    , attendeeCategory = "MAYBE COMING"
                    , displayPlusOne = True
                    }
            in
            Dict.get (attendeeStatusToString MaybeComing) attendeeDict
                |> Maybe.map (displayAttendees options)
                |> Maybe.withDefault (H.span [] [])

        notAttendingHtml =
            let
                options =
                    { testId = "view-attendees-not-attending-number"
                    , attendeeCategory = "CAN'T COME"
                    , displayPlusOne = False
                    }
            in
            Dict.get (attendeeStatusToString NotComing) attendeeDict
                |> Maybe.map (displayAttendees options)
                |> Maybe.withDefault (H.span [] [])
    in
    H.div [ A.class "guest-list-card" ]
        [ H.h2 [] [ H.text "Guest List" ]
        , H.text <| String.fromInt (List.length attendees) ++ " people have responded to the invitation"
        , attendingHtml
        , maybeAttendingHtml
        , notAttendingHtml
        ]


type alias Options =
    { testId : String
    , attendeeCategory : String
    , displayPlusOne : Bool
    }


displayAttendees : Options -> List Attendee -> Html msg
displayAttendees { testId, attendeeCategory, displayPlusOne } attendees =
    let
        ( attendeeComingCount, plusOnesCount ) =
            countAttendees attendees
    in
    H.div [ A.class "attendee-count-card" ]
        [ H.h4 [ A.class "attendee-count-header", A.attribute "data-testid" testId ]
            [ H.span [{- A.style "vertical-align" "middle" -}]
                [ H.text "• " ]
            , H.text
                (attendeeCategory
                    ++ " ("
                    ++ String.fromInt attendeeComingCount
                    ++ (if plusOnesCount > 0 && displayPlusOne then
                            " + " ++ String.fromInt plusOnesCount
                            -- ++ ")"

                        else
                            ""
                       )
                    ++ ")"
                )
            ]
        , H.ul [ A.class "attendee-flex-list" ]
            (List.map
                (\attendee ->
                    H.li [ A.class "attendee-name-wrapper" ]
                        [ H.span [ A.class "attendee-name" ]
                            [ H.text attendee.name
                            ]
                        , if attendee.plusOne && displayPlusOne then
                            H.span [ A.class "attendee-plus-one-bubble" ] [ H.text "+1" ]

                          else
                            H.span [] []
                        ]
                )
                attendees
            )
        ]



{-
   viewAttendees : List Attendee -> Html msg
   viewAttendees attendees =
       let
           attendeeDict =
               splitAttendees attendees

           attendingHtml =
               let
                   options =
                       { testId = "view-attendees-attending-number"
                       , attendeeCategory = "Attending"
                       , displayPlusOne = True
                       }
               in
               Dict.get (attendeeStatusToString Coming) attendeeDict
                   |> Maybe.map (displayAttendees options)
                   |> Maybe.withDefault (H.h3 [ A.attribute "data-testid" "view-attendees-attending-number" ] [ H.text "Attending: 0" ])

           maybeAttendingHtml =
               let
                   options =
                       { testId = "view-attendees-maybe-attending-number"
                       , attendeeCategory = "Maybe Attending"
                       , displayPlusOne = True
                       }
               in
               Dict.get (attendeeStatusToString MaybeComing) attendeeDict
                   |> Maybe.map (displayAttendees options)
                   |> Maybe.withDefault (H.span [] [])

           notAttendingHtml =
               let
                   options =
                       { testId = "view-attendees-not-attending-number"
                       , attendeeCategory = "Can't Attend"
                       , displayPlusOne = False
                       }
               in
               Dict.get (attendeeStatusToString NotComing) attendeeDict
                   |> Maybe.map (displayAttendees options)
                   |> Maybe.withDefault (H.span [] [])
       in
       H.div []
           [ attendingHtml
           , maybeAttendingHtml
           , notAttendingHtml
           ]


   type alias Options =
       { testId : String
       , attendeeCategory : String
       , displayPlusOne : Bool
       }


   displayAttendees : Options -> List Attendee -> Html msg
   displayAttendees { testId, attendeeCategory, displayPlusOne } attendees =
       let
           ( attendeeComingCount, plusOnesCount ) =
               countAttendees attendees
       in
       H.div [ A.class "attendee-count-card" ]
           [ H.h3 [ A.class "attendee-count-header", A.attribute "data-testid" testId ]
               [ H.text
                   (attendeeCategory
                       ++ ": "
                       ++ String.fromInt attendeeComingCount
                       ++ (if plusOnesCount > 0 && displayPlusOne then
                               " (+" ++ String.fromInt plusOnesCount ++ ")"

                           else
                               ""
                          )
                   )
               ]
           , H.div []
               (List.map
                   (\attendee ->
                       H.div [ A.class "attendee-name-wrapper" ]
                           [ H.div [ A.class "attendee-name" ]
                               [ H.text
                                   (attendee.name
                                       ++ (if attendee.plusOne && displayPlusOne then
                                               " (+1)"

                                           else
                                               ""
                                          )
                                   )
                               ]
                           ]
                   )
                   attendees
               )
           ]
-}


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
