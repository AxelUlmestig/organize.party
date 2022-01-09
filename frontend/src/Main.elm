module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Http
import Json.Decode as D -- exposing (Decoder, field, string)
import Url exposing (Url)
import Url.Parser as P exposing (Parser, (</>), int, map, oneOf, s, string)
import Url.Parser.Query as Q

type State
    = WaitingForInput String
    | Loading
    | Failure
    | Success Event
    | UrlStuff
    | NewEventState

type alias Event =
  { id : Int
  , title          : String
  , description    : String
  -- , startTime      : Time
  -- , endTime        : Time
  , location       : String
  -- , googleMapsLink : Maybe String
  }

eventDecoder : D.Decoder Event
eventDecoder = D.map4 Event
                 (D.field "id" D.int)
                 (D.field "title" D.string)
                 (D.field "description" D.string)
                 (D.field "location" D.string)

-- view : State -> Html Msg
view : State -> Browser.Document Msg
view state =
  Browser.Document "foo" [
    case state of
        WaitingForInput eventId ->
            H.div []
            [ H.input [ value eventId, onInput SetId ] []
            , H.button [ onClick (GetCat eventId) ] [ H.text "Submit"] ]

        Loading ->
            H.text "loading..."

        Failure ->
            H.text "failed to fetch new cat image"

        Success {title} ->
            H.text title
            -- img [ src imageUrl ] []

        UrlStuff -> H.text "url stuff"
        NewEventState -> H.h3 [] [H.text "Create A New Event"]
  ]


fetchCatImageUrl : String -> Cmd Msg
fetchCatImageUrl id =
    Http.get
        { url = "http://localhost:8081/api/v1/events/" ++ id
        -- , expect = Http.expectJson GotResult (field "file" string)
        , expect = Http.expectJson GotResult eventDecoder
        -- , expect = Http.expectString GotResult
        }


-- init : () -> ( State, Cmd Msg )
-- init _ = (WaitingForInput "", Cmd.none )
init : () -> Url -> Nav.Key -> ( State, Cmd Msg )
init _ url key = case P.parse routeParser url of
                   Just NewEvent -> ( NewEventState, Cmd.none )
                   Just (EventId id) -> ( Loading, fetchCatImageUrl id )
                   Nothing -> ( Failure, Cmd.none )

type Msg
    = GotResult (Result Http.Error Event)
    | GetCat String
    | SetId String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        SetId id -> ( WaitingForInput id, Cmd.none )
        GetCat id -> ( Loading, (fetchCatImageUrl id) )
        GotResult result ->
            case result of
                Ok event ->
                    ( Success event, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )
        UrlRequest _ -> ( UrlStuff, Cmd.none )
        UrlChange _ -> ( UrlStuff, Cmd.none )

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ P.map NewEvent P.top
    , P.map NewEvent (s "src" </> s "Main.elm") -- to make it work with elm reactor
    , P.map EventId (s "events" </> string)
    ]

type Route = NewEvent
           | EventId String

main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
