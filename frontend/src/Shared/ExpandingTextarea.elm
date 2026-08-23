module Shared.ExpandingTextarea exposing (expandingTextarea)

import Html exposing (Html, div, textarea)
import Html.Attributes as Attributes
import Html.Events as Events


expandingTextarea : { text : String, onInput : String -> msg, placeholder : String } -> Html msg
expandingTextarea { text, onInput, placeholder } =
    div
        [ Attributes.class "autoexpand" ]
        [ textarea
            [ Events.onInput onInput
            , Attributes.placeholder placeholder
            , Attributes.class "padded-input"
            , Attributes.attribute "data-testid" "expanding-text-area"
            ]
            [ Html.text text ]

        -- invisible sizer element, see the .autoexpand CSS
        , div
            [ Attributes.class "padded-input" ]
            [ Html.text (text ++ "_") ]
        ]
