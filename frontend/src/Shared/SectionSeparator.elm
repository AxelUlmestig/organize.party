module Shared.SectionSeparator exposing (sectionSeparator)

import Html as H exposing (Html)
import Html.Attributes as A


sectionSeparator : String -> Html a
sectionSeparator title =
    H.div [ A.class "section-separator" ]
        [ H.h5 [] [ H.text title ]
        , H.hr [] []
        ]
