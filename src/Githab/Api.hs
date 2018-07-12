{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Githab.Api
  ( gitlabApi
  , createMergeRequest
  , listBranches
  ) where

import Data.Proxy
import Githab.Types
import Servant.API
import qualified Servant.Client as ServantClient

type PrivateTokenHeader = Header' '[ Strict, Required] "Private-Token" PrivateToken

type GitlabApi
   = ReqBody '[ JSON] CreateMergeReq :> PrivateTokenHeader :> "projects" :> Capture "project-id" ProjectId :> "merge_requests" :> Post '[ JSON] MergeReq :<|> PrivateTokenHeader :> "projects" :> Capture "project-id" ProjectId :> "repository" :> "branches" :> Get '[ JSON] [Branch]

gitlabApi :: Proxy GitlabApi
gitlabApi = Proxy

createMergeRequest :: CreateMergeReq
                        -> PrivateToken -> ProjectId -> ServantClient.ClientM MergeReq
listBranches :: PrivateToken
                  -> ProjectId -> ServantClient.ClientM [Branch]
createMergeRequest :<|> listBranches = ServantClient.client gitlabApi
