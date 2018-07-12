{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Githab.Types ( PrivateToken (..)
                    , ProjectId (..)

                    , BranchName (..)
                    , branchNameToString

                    , HostExtractionException(..)

                    , Commit
                    , commitId
                    , commitShortId
                    , commitTitle
                    , commitCreatedAt
                    , commitMessage
                    , commitAuthorName
                    , commitAuthorEmail
                    , commitAuthoredDate
                    , commitCommitterName
                    , commitCommitterEmail
                    , commitCommittedDate

                    , Branch
                    , branchName
                    , branchCommit
                    , branchMerged
                    , branchProtected
                    , branchDevelopersCanPush
                    , branchDevelopersCanMerge

                    , MergeReq
                    , mrId
                    , mrIid
                    , mrTargetBranch
                    , mrSourceBranch
                    , mrProjectId
                    , mrTitle
                    , mrState
                    , mrWebUrl

                    , Env (..)
                    , HasEnv (..)

                    , CreateMergeReq
                    , mkCreateMergeReq
                    , createMrSourceBranch
                    , createMrTargetBranch
                    , createMrTitle
                    , createMrRemoveSourceBranch
                    ) where

import Cases (snakify)
import Control.Lens.TH (makeClassy, makeLenses)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time.LocalTime (ZonedTime(..))
import RIO
import qualified RIO.Text as T
import Servant.API (ToHttpApiData)
import Network.HTTP.Client (Manager)

newtype HostExtractionException =
  HostExtractionException Text
  deriving (Show)

instance Exception HostExtractionException

newtype PrivateToken = PrivateToken Text deriving (Show, Aeson.ToJSON, Aeson.FromJSON, ToHttpApiData, IsString)

newtype BranchName = BranchName Text deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)
branchNameToString :: BranchName -> String
branchNameToString (BranchName n) = T.unpack n

newtype ProjectId = ProjectId Text deriving (ToHttpApiData, IsString)

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
  { _branchName :: BranchName
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
  { _envAccessToken :: PrivateToken
  , _envLogFunc :: LogFunc
  , _envManager :: Manager
  }

makeClassy ''Env

instance HasLogFunc Env where
  logFuncL = envLogFunc

data CreateMergeReq = CreateMergeReq
  { _createMrSourceBranch :: BranchName
  , _createMrTargetBranch :: BranchName
  , _createMrTitle :: Text
  , _createMrRemoveSourceBranch :: Bool
  } deriving (Show)

mkCreateMergeReq :: BranchName -> BranchName -> Text -> Bool -> CreateMergeReq
mkCreateMergeReq = CreateMergeReq

makeLenses ''CreateMergeReq

$(deriveJSON
    (defaultOptions
       { Aeson.fieldLabelModifier =
           T.unpack . snakify . T.pack . drop (length @[] "_createMr")
       })
    'CreateMergeReq)
