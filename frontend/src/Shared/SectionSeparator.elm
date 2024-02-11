module Shared.SectionSeparator exposing (sectionSeparator)

import Html as H exposing (Html)
import Html.Attributes as A


sectionSeparator : String -> Html a
sectionSeparator title =
    H.div [ A.class "d-flex flex-row justify-content-start section-separator", A.style "margin-top" "1rem" ]
        [ H.h5 [ A.class "mb-4", A.style "white-space" "nowrap" ] [ H.text title ]
        , H.hr [ A.style "width" "100%", A.style "margin-left" "1rem", A.style "z-index" "-1" ] []
        ]
