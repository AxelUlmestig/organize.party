module Shared.PhotoUploader exposing
    ( Msg(..)
      -- , OutMsg
    , State
    , clear
    , getPhotoUrl
    , init
    , newPhoto
    , submitPhoto
    , update
    , view
    )

import File exposing (File)
import File.Select
import Html as H exposing (Html)
import Html.Attributes as A
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Process
import Task
import Url exposing (Url)


type State
    = Initialized { photo : Maybe File, photoUrl : Maybe String }
    | WaitingForUploadUrl { photo : File, photoUploadId : String }
    | WaitingForPhotoId { photo : File, photoUploadId : String }
    | Completed { photo : File, photoId : String }
    | Failure


type Msg
    = PhotoUploadDone (Maybe String)
    | InternalMsg InternalMsg


type InternalMsg
    = {- FileRequested
         |
      -}
      FileSelected File
    | PhotoUrlGenerated String
    | PhotoUploadInitiated File (Result Http.Error PhotoUpload)
    | PollForUploadUrl
    | PhotoUploaded (Result Http.Error ())
    | PollForPhotoId
    | PolledForPhotoId (Result Http.Error PhotoUpload)



{-
   type OutMsg
       = PhotoUploadDone (Maybe String)
       | UploadFailed String
-}


type alias PhotoUpload =
    { id : String
    , uploadUrl : Url
    , photoId : Maybe String
    , materializedStatus : String
    }


init : ( State, Cmd Msg )
init =
    ( Initialized { photo = Nothing, photoUrl = Nothing }, Cmd.none )


photoUploadDecoder =
    D.succeed PhotoUpload
        |> required "id" D.string
        |> required "uploadUrl" urlDecoder
        |> required "photoId" (D.nullable D.string)
        |> required "materializedStatus" D.string


update : InternalMsg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        {-
           FileRequested ->
               ( state
               , File.Select.file [ "image/jpg", "image/jpeg", "image/png" ] (InternalMsg << FileSelected)
               , Nothing
               )
        -}
        FileSelected file ->
            -- TODO: verify file size
            ( Initialized { photo = Just file, photoUrl = Nothing }
            , Task.perform (InternalMsg << PhotoUrlGenerated) (File.toUrl file)
            )

        PhotoUrlGenerated photoUrl ->
            case state of
                Initialized { photo } ->
                    ( Initialized { photo = photo, photoUrl = Just photoUrl }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        PhotoUploadInitiated file result ->
            case result of
                Ok { id, uploadUrl } ->
                    let
                        newState =
                            WaitingForPhotoId { photoUploadId = id, photo = file }

                        cmd =
                            Http.request
                                { method = "PUT"
                                , url = Url.toString uploadUrl
                                , body = Http.fileBody file
                                , expect = Http.expectWhatever (InternalMsg << PhotoUploaded)
                                , headers = [ Http.header "Content-Type" (File.mime file) ]
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    in
                    ( newState, cmd )

                -- TODO: better error handling
                Err err ->
                    ( Failure, Cmd.none )

        PollForUploadUrl ->
            case state of
                WaitingForUploadUrl { photo, photoUploadId } ->
                    let
                        cmd =
                            Http.get
                                { url = "/api/v1/photo-upload/" ++ photoUploadId
                                , expect = Http.expectJson (InternalMsg << PhotoUploadInitiated photo) photoUploadDecoder
                                }
                    in
                    ( state, cmd )

                _ ->
                    ( state, Cmd.none )

        PhotoUploaded result ->
            case result of
                Ok () ->
                    case state of
                        WaitingForPhotoId photoState ->
                            let
                                cmd =
                                    Http.get
                                        { url = "/api/v1/photo-upload/" ++ photoState.photoUploadId
                                        , expect = Http.expectJson (InternalMsg << PolledForPhotoId) photoUploadDecoder
                                        }
                            in
                            ( state, cmd )

                        _ ->
                            ( state, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        PolledForPhotoId result ->
            case state of
                WaitingForPhotoId { photoUploadId, photo } ->
                    case result of
                        Ok photoUpload ->
                            case photoUpload.photoId of
                                Nothing ->
                                    let
                                        cmd =
                                            Process.sleep 250 |> Task.perform (\_ -> InternalMsg PollForPhotoId)
                                    in
                                    ( state, cmd )

                                Just photoId ->
                                    let
                                        newState =
                                            Completed { photoId = photoId, photo = photo }
                                    in
                                    ( newState, pureCmd (PhotoUploadDone (Just photoId)) )

                        Err err ->
                            ( Failure, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        PollForPhotoId ->
            case state of
                WaitingForPhotoId { photoUploadId } ->
                    let
                        cmd =
                            Http.get
                                { url = "/api/v1/photo-upload/" ++ photoUploadId
                                , expect = Http.expectJson (InternalMsg << PolledForPhotoId) photoUploadDecoder
                                }
                    in
                    ( state, cmd )

                _ ->
                    ( state, Cmd.none )


view : Maybe String -> Html Msg
view photoId =
    H.div [] []


newPhoto : State -> ( State, Cmd Msg )
newPhoto state =
    ( state
    , File.Select.file [ "image/jpg", "image/jpeg", "image/png" ] (InternalMsg << FileSelected)
    )


getPhotoUrl : State -> Maybe String
getPhotoUrl state =
    case state of
        Initialized { photoUrl } ->
            photoUrl

        _ ->
            Nothing


clear : State -> ( State, Cmd Msg )
clear _ =
    ( Initialized { photo = Nothing, photoUrl = Nothing }, Cmd.none )


submitPhoto : State -> ( State, Cmd Msg )
submitPhoto state =
    case state of
        Initialized initState ->
            case initState.photo of
                Nothing ->
                    -- ( state, Cmd.none )
                    ( state, pureCmd (PhotoUploadDone Nothing) )

                Just photo ->
                    let
                        body =
                            Encode.object [ ( "fileName", Encode.string (File.name photo) ) ]

                        cmd =
                            Http.post
                                { url = "/api/v1/photo-upload"
                                , expect = Http.expectJson (InternalMsg << PhotoUploadInitiated photo) photoUploadDecoder
                                , body = Http.jsonBody body
                                }
                    in
                    ( state, cmd )

        _ ->
            ( state, Cmd.none )



-- TODO: handle error here?


urlDecoder : D.Decoder Url
urlDecoder =
    D.string
        |> D.andThen
            (\urlString ->
                case Url.fromString urlString of
                    Just url ->
                        D.succeed url

                    Nothing ->
                        D.fail ("Invalid URL: " ++ urlString)
            )


pureCmd : msg -> Cmd msg
pureCmd =
    Task.perform identity << Task.succeed
