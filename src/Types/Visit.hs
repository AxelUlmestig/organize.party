{-# LANGUAGE FlexibleInstances #-}

module Types.Visit (Visit(..), VisitStatus(..), fromTuple, toTuple, readStatus, writeStatus) where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Int                (Int64)
import           Data.String.Interpolate (iii)
import           Data.Text               (Text, pack, toTitle, unpack)
import           Data.Time               (UTCTime)
import           Data.UUID               (UUID)
import           GHC.Generics            (Generic)

data VisitStatus = Coming
                 | MaybeComing
                 | NotComing
                 deriving (Eq, Read, Generic, Show)

instance ToJSON VisitStatus
instance FromJSON VisitStatus

data Visit = Visit
           { eventId   :: UUID
           , email     :: Text
           , firstName :: Text
           , lastName  :: Text
           , status    :: VisitStatus
           , plusOne   :: Bool
           , rsvpAt    :: UTCTime
           }
           deriving (Generic, Show)

instance ToJSON Visit

writeStatus :: VisitStatus -> Text
writeStatus Coming      = "coming"
writeStatus MaybeComing = "maybe_coming"
writeStatus NotComing   = "not_coming"

readStatus :: Text -> VisitStatus
readStatus "coming"       = Coming
readStatus "maybe_coming" = MaybeComing
readStatus "not_coming"   = NotComing
readStatus other          = error [iii|unknown VisitStatus: #{other}|]

fromTuple :: (UUID, Text, Text, Text, Text, Bool, UTCTime) -> Visit
fromTuple (eventId, email, firstName, lastName, status, plusOne, rsvpAt) = Visit eventId email firstName lastName (readStatus status) plusOne rsvpAt

toTuple :: Visit -> (UUID, Text, Text, Text, Text, Bool)
toTuple Visit{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)
