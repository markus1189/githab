{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Githab.Types where

import Cases (snakify)
import Control.Lens.TH (makeClassy, makeLenses)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time.LocalTime (ZonedTime(..))
import qualified Network.Wreq.Session as WreqS
import RIO
import qualified RIO.Text as T
import Servant.API

newtype HostExtractionException =
  HostExtractionException Text
  deriving (Show)

instance Exception HostExtractionException

data Commit = Commit
  { _commitId :: Text
  , _commitShortId :: Text
  , _commitTitle :: Text
  , _commitCreatedAt :: ZonedTime
  , _commitMessage :: Text
  , _commitAuthorName :: Text
  , _commitAuthorEmail :: Text
  , _commitAuthoredDate :: ZonedTime
  , _commitCommitterName :: Text
  , _commitCommitterEmail :: Text
  , _commitCommittedDate :: ZonedTime
  } deriving (Show)

makeLenses ''Commit

$(deriveJSON
    (defaultOptions
       { Aeson.fieldLabelModifier =
           T.unpack . snakify . T.pack . drop (length @[] "_commit")
       })
    'Commit)

data Branch = Branch
  { _branchName :: Text
  , _branchCommit :: Commit
  , _branchMerged :: Bool
  , _branchProtected :: Bool
  , _branchDevelopersCanPush :: Bool
  , _branchDevelopersCanMerge :: Bool
  } deriving (Show)

makeLenses ''Branch

$(deriveJSON
    (defaultOptions
       { Aeson.fieldLabelModifier =
           T.unpack . snakify . T.pack . drop (length @[] "_branch")
       })
    'Branch)

data MergeReq = MergeReq
  { _mrId :: Int
  , _mrIid :: Int
  , _mrTargetBranch :: Text
  , _mrSourceBranch :: Text
  , _mrProjectId :: Int
  , _mrTitle :: Text
  , _mrState :: Text
  , _mrWebUrl :: Text
  } deriving (Show)

makeLenses ''MergeReq

$(deriveJSON
    (defaultOptions
       { Aeson.fieldLabelModifier =
           T.unpack . snakify . T.pack . drop (length @[] "_mr")
       })
    'MergeReq)

data Env = Env
  { _envAccessToken :: String
  , _envLogFunc :: LogFunc
  , _envSession :: WreqS.Session
  }

makeClassy ''Env

instance HasLogFunc Env where
  logFuncL = envLogFunc

data CreateMergeReq = CreateMergeReq
  { _createMrSourceBranch :: Text
  , _createMrTargetBranch :: Text
  , _createMrTitle :: Text
  , _createMrRemoveSourceBranch :: Bool
  } deriving (Show)

$(deriveJSON
    (defaultOptions
       { Aeson.fieldLabelModifier =
           T.unpack . snakify . T.pack . drop (length @[] "_createMr")
       })
    'CreateMergeReq)

type PrivateTokenHeader = Header' '[ Strict, Required] "Private-Token" Text

type API
   = PrivateTokenHeader :> "projects" :> Capture "project-id" Text :> "merge_requests" :> ReqBody '[ JSON] CreateMergeReq :> Post '[ JSON] MergeReq :<|> PrivateTokenHeader :> "projects" :> Capture "project-id" Text :> "repository" :> "branches" :> Get '[ JSON] [Branch]
