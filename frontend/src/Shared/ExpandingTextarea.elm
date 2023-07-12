module Shared.ExpandingTextarea exposing (expandingTextarea)

import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes as Attributes
import Html.Events as Events

expandingTextarea { text, onInput, placeholder, styling } =
  div
    [ Attributes.class "autoexpand" ]
    [ textarea
        [ Events.onInput onInput
        , Attributes.placeholder placeholder
        , Attributes.class "padded-input"
        ] [ Html.text text ]
    , div
        [ Attributes.class "padded-input"
        ]
        [Html.text (text ++ "_")]
    ]
