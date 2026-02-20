{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ULP.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

data ChannelState = Down | Opening | Open | Closing | Closed
  deriving (Show, Eq, Generic)

instance ToJSON ChannelState
instance FromJSON ChannelState

data CommitStatus = Pending | Validated | Sealed | Quarantined
  deriving (Show, Eq, Generic)

instance ToJSON CommitStatus
instance FromJSON CommitStatus

data FaceStatus = Pass | Fail | Unknown
  deriving (Show, Eq, Generic)

instance ToJSON FaceStatus
instance FromJSON FaceStatus

data CommitType = VertexInit | EdgeUpdate | FaceEval | Commit | Projection | Sync
  deriving (Show, Eq, Generic)

instance ToJSON CommitType
instance FromJSON CommitType

data VertexIdentity = VertexIdentity
  { vertex_id :: Text
  , path :: Text
  , address :: Text
  , pubkey :: Text
  , fano_point_id :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON VertexIdentity
instance FromJSON VertexIdentity

data EdgeState = EdgeState
  { edge_id :: Text
  , from :: Text
  , to :: Text
  , channel_state :: ChannelState
  , last_seq :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON EdgeState
instance FromJSON EdgeState

data FaceInvariant = FaceInvariant
  { face_id :: Text
  , vertices :: [Text]
  , invariant_name :: Text
  , status :: FaceStatus
  , evidence :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON FaceInvariant
instance FromJSON FaceInvariant

data CentroidState = CentroidState
  { stop_metric :: Double
  , closure_ratio :: Double
  , sabbath :: Bool
  , reason :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CentroidState
instance FromJSON CentroidState

data Merkle = Merkle
  { version :: Text
  , sections :: [(Text, Text)]
  , leaf_order :: [Text]
  , root :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Merkle
instance FromJSON Merkle

data CommitEvent = CommitEvent
  { cid :: Text
  , t :: Int64
  , lc :: Maybe Int
  , ctype :: CommitType
  , parents :: [Text]
  , identities :: Maybe [VertexIdentity]
  , vertex :: Maybe VertexIdentity
  , edges :: [EdgeState]
  , faces :: [FaceInvariant]
  , centroid :: CentroidState
  , cstatus :: CommitStatus
  , prev_hash :: Maybe Text
  , merkle :: Maybe Merkle
  , self_hash :: Text
  , sig :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CommitEvent
instance FromJSON CommitEvent

data ValidationResult = ValidationResult
  { valid :: Bool
  , errors :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationResult
instance FromJSON ValidationResult

-- Runtime options and hooks
data ValidationOptions = ValidationOptions
  { signatureVerifier :: Maybe (CommitEvent -> Text -> IO Bool)
  , invariantChecker :: Maybe (CommitEvent -> IO Bool)
  }

data RuntimeOptions = RuntimeOptions
  { clock :: IO Int64
  , counterStart :: Int
  , signer :: Maybe (CommitEvent -> Text -> IO Text)
  , verifier :: Maybe (CommitEvent -> Text -> IO Bool)
  , storageRoot :: FilePath
  }
