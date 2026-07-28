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
import SHA256
import Task
import Types
import Url exposing (Url)


type alias PhotoDetails =
    { file : File
    , url : String
    , sha256Hash : String
    }


type State
    = NoFileSelected
    | ExistingPhoto Types.Photo
    | PreparingFile { photo : Maybe File, photoUrl : Maybe String }
    | FileReady { photo : PhotoDetails }
    | WaitingForUploadUrl { photo : PhotoDetails, photoUploadId : String }
    | WaitingForPhotoId { photo : PhotoDetails, photoUploadId : String }
    | Completed { photo : PhotoDetails, photoId : String }
    | Failure


type Msg
    = PhotoUploadDone (Maybe String)
    | InternalMsg InternalMsg


type InternalMsg
    = FileSelected File
    | PhotoUrlGenerated String
    | PhotoHashGenerated String
    | PhotoUploadInitiated (Result Http.Error InitPhotoUploadResponse)
    | PollForUploadUrl
    | PhotoUploaded (Result Http.Error ())
    | PollForPhotoId
    | PolledForPhotoId (Result Http.Error PhotoUploadStatus)


type alias InitPhotoUploadResponse =
    { id : Maybe String
    , uploadUrl : Maybe Url
    , photoId : Maybe String
    , materializedStatus : String
    }


initPhotoUploadResponseDecoder =
    D.succeed InitPhotoUploadResponse
        |> required "id" (D.nullable D.string)
        |> required "uploadUrl" (D.nullable urlDecoder)
        |> required "photoId" (D.nullable D.string)
        |> required "materializedStatus" D.string


type alias PhotoUploadStatus =
    { id : String
    , uploadUrl : Url
    , photoId : Maybe String
    , materializedStatus : String
    }


photoUploadStatusDecoder =
    D.succeed PhotoUploadStatus
        |> required "id" D.string
        |> required "uploadUrl" urlDecoder
        |> required "photoId" (D.nullable D.string)
        |> required "materializedStatus" D.string


init : Maybe Types.Photo -> ( State, Cmd Msg )
init mPhoto =
    case mPhoto of
        Nothing ->
            ( NoFileSelected, Cmd.none )

        Just photo ->
            ( ExistingPhoto photo, Cmd.none )


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
            ( PreparingFile { photo = Just file, photoUrl = Nothing }
            , Task.perform (InternalMsg << PhotoUrlGenerated) (File.toUrl file)
            )

        PhotoUrlGenerated photoUrl ->
            case state of
                PreparingFile { photo } ->
                    case photo of
                        Just file ->
                            ( PreparingFile { photo = Just file, photoUrl = Just photoUrl }
                            , Task.perform (InternalMsg << PhotoHashGenerated) (Task.map (SHA256.toBase64 << SHA256.fromBytes) (File.toBytes file))
                            )

                        Nothing ->
                            ( state, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        PhotoHashGenerated sha256Hash ->
            case state of
                PreparingFile { photo, photoUrl } ->
                    case ( photo, photoUrl ) of
                        ( Just file, Just url ) ->
                            ( FileReady { photo = { file = file, url = url, sha256Hash = sha256Hash } }
                            , Cmd.none
                            )

                        _ ->
                            ( state, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        PhotoUploadInitiated response ->
            case state of
                FileReady { photo } ->
                    case response of
                        Ok result ->
                            -- { id, uploadUrl, materializedStatus, photoId } ->
                            case ( result.photoId, result.id, result.uploadUrl ) of
                                ( Just photoId, _, _ ) ->
                                    let
                                        newState =
                                            Completed { photoId = photoId, photo = photo }
                                    in
                                    ( newState, pureCmd (PhotoUploadDone (Just photoId)) )

                                ( _, Just id, Just uploadUrl ) ->
                                    let
                                        newState =
                                            WaitingForPhotoId { photoUploadId = id, photo = photo }

                                        cmd =
                                            Http.request
                                                { method = "PUT"
                                                , url = Url.toString uploadUrl
                                                , body = Http.fileBody photo.file
                                                , expect = Http.expectWhatever (InternalMsg << PhotoUploaded)
                                                , headers = [ Http.header "Content-Type" (File.mime photo.file) ]
                                                , timeout = Nothing
                                                , tracker = Nothing
                                                }
                                    in
                                    ( newState, cmd )

                                -- TODO: better error handling
                                _ ->
                                    ( Failure, Cmd.none )

                        -- TODO: Handle 409 case
                        Err err ->
                            ( Failure, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        PollForUploadUrl ->
            case state of
                WaitingForUploadUrl { photo, photoUploadId } ->
                    let
                        cmd =
                            Http.get
                                { url = "/api/v1/photo-upload/" ++ photoUploadId
                                , expect = Http.expectJson (InternalMsg << PolledForPhotoId) photoUploadStatusDecoder
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
                                        , expect = Http.expectJson (InternalMsg << PolledForPhotoId) photoUploadStatusDecoder
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
                                , expect = Http.expectJson (InternalMsg << PolledForPhotoId) photoUploadStatusDecoder
                                }
                    in
                    ( state, cmd )

                _ ->
                    ( state, Cmd.none )


view : State -> Html a
view state =
    case state of
        NoFileSelected ->
            H.text "Waiting for input"

        ExistingPhoto _ ->
            H.text "File ready for upload"

        PreparingFile _ ->
            H.text "Preparing file"

        FileReady _ ->
            H.text "File ready for upload"

        WaitingForUploadUrl _ ->
            H.text "Fetching photo upload info..."

        WaitingForPhotoId _ ->
            H.text "Waiting for photo upload confirmation..."

        Completed _ ->
            H.text "Photo upload completed"

        Failure ->
            H.text "Photo upload failed"


newPhoto : State -> ( State, Cmd Msg )
newPhoto state =
    ( state
    , File.Select.file [ "image/jpg", "image/jpeg", "image/png" ] (InternalMsg << FileSelected)
    )


getPhotoUrl : State -> Maybe String
getPhotoUrl state =
    case state of
        FileReady { photo } ->
            Just photo.url

        ExistingPhoto photo ->
            Just (Url.toString photo.url)

        _ ->
            Nothing


clear : State -> ( State, Cmd Msg )
clear _ =
    ( NoFileSelected, Cmd.none )


submitPhoto : State -> ( State, Cmd Msg )
submitPhoto state =
    case state of
        NoFileSelected ->
            ( state, pureCmd (PhotoUploadDone Nothing) )

        ExistingPhoto photo ->
            ( state, pureCmd (PhotoUploadDone (Just photo.id)) )

        FileReady { photo } ->
            let
                body =
                    Encode.object
                        [ ( "fileName", Encode.string (File.name photo.file) )
                        , ( "base64Sha256", Encode.string photo.sha256Hash )
                        ]

                cmd =
                    Http.post
                        { url = "/api/v1/photo-upload"
                        , expect = Http.expectJson (InternalMsg << PhotoUploadInitiated) initPhotoUploadResponseDecoder
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
