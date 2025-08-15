module Shared.Navbar exposing
    ( NavbarMsg(..)
    , NavbarState
    , init
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events exposing (on, onCheck, onClick, onInput)
import Types exposing (..)


type NavbarState
    = NavbarOpen
    | NavbarClosed


type NavbarMsg
    = CloseNavbar
    | OpenNavbar


view : NavbarState -> Html NavbarMsg
view state =
    H.nav [ A.class "navbar" ]
        [ H.div [ A.class "navbar-container container" ]
            [ H.input
                [ A.type_ "checkbox"
                , A.checked (navbarStateToBool state)
                , onCheck
                    (\checked ->
                        if checked then
                            OpenNavbar

                        else
                            CloseNavbar
                    )
                ]
                []
            , H.div [ A.class "hamburger-lines" ]
                [ H.span [ A.class "line line1" ] []
                , H.span [ A.class "line line2" ] []
                , H.span [ A.class "line line3" ] []
                ]
            , H.ul [ A.class "menu-items" ]
                [ H.li [] [ H.a [ A.href "/", onClick CloseNavbar ] [ H.text "Create a new event" ] ]
                , H.li [] [ H.a [ A.href "/forget-me", onClick CloseNavbar ] [ H.text "Forget Me" ] ]
                , H.li [] [ H.a [ A.href "/about", onClick CloseNavbar ] [ H.text "About" ] ]
                ]
            , H.a
                [ A.href "/", A.class "logo" ]
                [ H.img [ A.src "./logo.svg", A.alt "organize.party" ] [] ]
            ]
        ]


update : NavbarMsg -> NavbarState -> ( NavbarState, Cmd NavbarMsg )
update msg model =
    case msg of
        OpenNavbar ->
            ( NavbarOpen, Cmd.none )

        CloseNavbar ->
            ( NavbarClosed, Cmd.none )


navbarStateToBool : NavbarState -> Bool
navbarStateToBool state =
    case state of
        NavbarOpen ->
            True

        NavbarClosed ->
            False


init : NavbarState
init =
    NavbarClosed
