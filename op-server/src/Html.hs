module Html (
  HTML,
  RawHtml,
  frontPage,
  eventPage
) where


import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.ByteString.Lazy.Search (replace)
import           Data.ByteString.UTF8        as BSU
import           Data.String.Interpolate     (__i)
import           Data.UUID                   (UUID)
import qualified Endpoints.GetEvent
import           Network.HTTP.Media          ((//), (/:))
import           Servant
import           Servant.API
import           Types.AppEnv                (AppEnv (..), SmtpConfig (..))
import           Types.Event                 (Event (..))

-- type shenanigans to enable serving raw html

data HTML

newtype RawHtml = RawHtml { unRaw :: LBS.ByteString }

instance Accept HTML where
  contentTypes _ = pure $ "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

--

indexHtmlPath :: FilePath
indexHtmlPath = "frontend/index.html"

openGraphPlaceHolder :: ByteString
openGraphPlaceHolder = "<!-- OPEN_GRAPH_PLACEHOLDER -->"

frontPage ::
  ( MonadIO m
  , MonadReader AppEnv m
  ) =>
  m RawHtml
frontPage = do
  rawIndexHtml <- liftIO $ LBS.readFile indexHtmlPath
  hostUrl <- asks hostUrl

  let indexHtmlWithOpenGraph =
          replace
              openGraphPlaceHolder
              (frontPageOg hostUrl)
              rawIndexHtml

  pure $ RawHtml indexHtmlWithOpenGraph

frontPageOg :: String -> LBS.ByteString
frontPageOg hostUrl =
  [__i|
    <meta property="og:title" content="organize.party" />
    <meta property="og:type" content="website" />
    <meta property="og:url" content="#{hostUrl}" />
    <meta property="og:image" content="#{hostUrl}/logo.svg" />
    <meta property="og:image:type" content="image/svg+xml" />
    <meta property="og:description" content="Create an event and invite your friends today!" />
  |]

eventPage ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader AppEnv m
  ) =>
  UUID ->
  m RawHtml
eventPage eventId = do
  rawIndexHtml <- liftIO $ LBS.readFile indexHtmlPath
  hostUrl <- asks hostUrl

  openGraphData <- do
    mEvent <- Endpoints.GetEvent.maybeGetEvent eventId
    pure $ case mEvent of
      Just event -> eventPageOg event hostUrl
      Nothing    -> frontPageOg hostUrl


  let indexHtmlWithOpenGraph =
        replace
          openGraphPlaceHolder
          openGraphData
          rawIndexHtml

  pure $ RawHtml indexHtmlWithOpenGraph
  where
    eventPageOg Event{title, description} hostUrl =
      [__i|
        <meta property="og:title" content="#{title}" />
        <meta property="og:type" content="website" />
        <meta property="og:url" content="#{hostUrl}/e/#{eventId}" />
        <meta property="og:image" content="#{hostUrl}/logo.svg" />
        <meta property="og:image:type" content="image/svg+xml" />
        <meta property="og:description" content="#{description}" />
      |] :: LBS.ByteString

