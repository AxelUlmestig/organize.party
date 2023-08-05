module Shared.FormatUrls exposing (formatTextWithLinks)

import Parser exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import List

formatTextWithLinks : String -> Html msg
formatTextWithLinks input =
  case run parser input of
    Err _ -> H.text input
    Ok result -> result

parser : Parser (Html msg)
parser =
  let linkOrNotLink textPieces =
        oneOf
          [ succeed (\textPiece -> Loop (consTextPiece textPiece textPieces))
            |= oneOf
              [ linkParser
              , anyCharParser
              ]
          , succeed (\_ -> Done (H.span [] <| List.map textPieceToHtml <| List.reverse textPieces)) |= end
          ]
  in loop [] linkOrNotLink

type TextPiece
  = Normal String
  | Link String

consTextPiece : TextPiece -> List TextPiece -> List TextPiece
consTextPiece tp tps =
  case (tp, tps) of
    (Normal s1, (Normal s2 :: tpss)) -> Normal (s2 ++ s1) :: tpss
    _ -> tp :: tps

textPieceToHtml : TextPiece -> Html msg
textPieceToHtml tp =
  case tp of
    Normal s -> H.text s
    Link l -> H.a [ A.href l ] [ H.text l ]


linkParser : Parser TextPiece
linkParser =
  succeed (\protocol link -> Link (protocol ++ link))
    |= oneOf
      [ map (\_ -> "http://") (token "http://")
      , map (\_ -> "https://") (token "https://")
      ]
    |= (getChompedString <| chompWhile (\c -> c /= ' '))

anyCharParser : Parser TextPiece
anyCharParser =
  succeed Normal
    |= (getChompedString <| chompIf (\_ -> True))
