module Shared.Navbar exposing (navbar)

import Html as H exposing (Html)
import Html.Attributes as A

navbar : Html msg
navbar =
    H.nav [ A.class "navbar" ]
      [ H.div [ A.class "navbar-container container" ]
        [ H.input [ A.type_ "checkbox", A.name "", A.id "" ] []
        , H.div [ A.class "hamburger-lines" ]
            [ H.span [ A.class "line line1" ] []
            , H.span [ A.class "line line2" ] []
            , H.span [ A.class "line line3" ] []
            ]
        , H.ul [ A.class "menu-items" ]
          [ H.li [] [ H.a [ A.href "/" ] [ H.text "Create a new event" ] ]
          , H.li [] [ H.a [ A.href "/about" ] [ H.text "About" ] ]
          ]
        , H.h1 [ A.class "logo" ] [ H.a [ A.href "/" ] [ H.text "organize.party" ] ]
        ]
      ]
