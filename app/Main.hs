{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Applicative ((<**>))
import Control.Exception (throw)
import Control.Lens (_Just, preview, toListOf)
import Control.Lens.TH (makePrisms)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as AesonLens
import Data.Foldable (traverse_)
import Data.List (find, sortOn)
import Data.Proxy
import Data.Text.Encoding.Error (strictDecode)
import Data.Time.LocalTime (ZonedTime(..))
import qualified FortyTwo
import qualified FortyTwo.Utils as FortyTwo
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client
  ( HttpException(..)
  , HttpExceptionContent(..)
  , ManagerSettings
  , Response(..)
  , defaultManagerSettings
  )
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (urlEncode)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as WreqS
import qualified Options.Applicative as OA
import RIO
import qualified RIO.Text as T
import Servant.API
import qualified Servant.Client as ServantClient
import qualified Turtle
import Githab.Types

makePrisms ''HttpException

makePrisms ''HttpExceptionContent

data Options = Options
  { _optionsToken :: String
  , _optionsVerbose :: Bool
  , _optionsNoVerify :: Bool
  , _optionsIntoMaster :: Bool
  }

options :: OA.Parser Options
options =
  Options <$>
  OA.strOption
    (OA.long "token" <> OA.metavar "TOKEN" <> OA.help "Access token for gitlab") <*>
  OA.switch
    (OA.long "verbose" <> OA.short 'v' <> OA.help "Enable verbose logging") <*>
  OA.switch
    (OA.long "no-verify" <>
     OA.help "Disable certificate check (on your own risk)") <*>
  OA.switch
    (OA.long "into-master" <>
     OA.help "Use master as the target branch and don't ask")

opts :: OA.ParserInfo Options
opts =
  OA.info
    (options <**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Create a merge request in gitlab")

api :: Proxy API
api = Proxy

servantMergeReq ::
     Text -> Text -> CreateMergeReq -> ServantClient.ClientM MergeReq
servantBranches :: Text -> Text -> ServantClient.ClientM [Branch]
servantMergeReq :<|> servantBranches = ServantClient.client api

baseUrl :: String
baseUrl = "https://gitlab.com/api/v4"

mergeRequestEndpointFor :: String -> String
mergeRequestEndpointFor ident = "/projects/" ++ ident ++ "/merge_requests"

branchesEndpointFor :: String -> String
branchesEndpointFor ident = "/projects/" ++ ident ++ "/repository/branches"

createMrBody :: Text -> Text -> Text -> Value
createMrBody title source target =
  Aeson.object
    [ ("source_branch", Aeson.String source)
    , ("target_branch", Aeson.String target)
    , ("title", Aeson.String title)
    , ("remove_source_branch", Aeson.Bool True)
    ]

main :: IO ()
main = do
  Options token isVerbose noVerify intoMaster <- OA.execParser opts
  session <-
    WreqS.newSessionControl
      Nothing
      (if noVerify
         then noVerifyTlsManagerSettings
         else defaultManagerSettings)
  logOptions' <- logOptionsHandle stderr isVerbose
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    let app = Env token lf session
    runRIO app $ do
      when noVerify $ logDebug "Certificate checking is disabled!"
      eitherBranches <- getBranches
      case eitherBranches of
        Left e -> handleHttpException e
        Right bs -> do
          curBranch <- getCurrentBranch
          let bs' = toListOf (traverse . branchName) bs
          sourceBranch <-
            if curBranch `elem` bs'
              then liftIO $
                   FortyTwo.selectWithDefault
                     "Source branch?"
                     (map T.unpack bs')
                     (T.unpack curBranch)
              else liftIO $ FortyTwo.select "Source branch?" (map T.unpack bs')
          liftIO FortyTwo.flush
          if not (null sourceBranch)
            then do
              targetBranch <-
                if intoMaster
                  then return "master"
                  else liftIO $
                       FortyTwo.selectWithDefault
                         "Target branch?"
                         (map T.unpack bs')
                         "master"
              title <- titlePrompt sourceBranch bs
              createMergeRequest
                (T.pack title)
                (T.pack sourceBranch)
                (T.pack targetBranch)
            else logError "No source branch given!"

shellCmd :: MonadIO m => Text -> m Text
shellCmd c = Turtle.lineToText <$> Turtle.single (Turtle.inshell c Turtle.empty)

getCurrentBranch :: MonadIO m => m Text
getCurrentBranch = liftIO $ shellCmd "git rev-parse --abbrev-ref HEAD"

getHostAndProject :: MonadIO m => m (Text, Text)
getHostAndProject = do
  fullUrl <- shellCmd "git remote get-url origin"
  (host, project) <- splitRemoteOrFail fullUrl
  return
    ( host
    , T.decodeUtf8With strictDecode . urlEncode True . encodeUtf8 $ project)

splitRemoteOrFail :: MonadIO m => Text -> m (Text, Text)
splitRemoteOrFail url =
  case T.split (\c -> elem @[] c "@:") url of
    [_, host, projectWithSuffix] -> return (host, T.dropEnd 4 projectWithSuffix)
    _ -> throwIO (HostExtractionException url)

createMergeRequest ::
     ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     , MonadUnliftIO m
     , HasEnv env
     , HasCallStack
     )
  => Text
  -> Text
  -> Text
  -> m ()
createMergeRequest title source target = do
  sess <- view envSession
  (T.unpack -> host, T.unpack -> project) <- getHostAndProject
  if source == target
    then logError "Source and target branches are equal, refusing."
    else do
      result <-
        try $ do
          liftIO FortyTwo.flush
          token <- view envAccessToken
          response <-
            liftIO $
            WreqS.post
              sess
              ("https://" ++
               host ++
               "/api/v4" ++
               mergeRequestEndpointFor project ++ "?private_token=" ++ token)
              (createMrBody title source target)
          return (preview (Wreq.responseBody . AesonLens._JSON) response)
      case result of
        Right (Just mr) -> do
          logInfo . display $ view mrWebUrl mr
          logInfo "Created the merge request"
        Right Nothing -> logError . displayShow $ result
        Left e -> handleHttpException e

titlePrompt :: MonadIO m => String -> [Branch] -> m String
titlePrompt source bs =
  liftIO $
  FortyTwo.selectWithDefault
    "Title?"
    (suggested : maybeToList fromCommit)
    defaultTitle <*
  FortyTwo.flush
  where
    defaultTitle = fromMaybe ("Merge branch " ++ source) fromCommit
    suggested = "Merge branch " ++ source
    fromCommit =
      preview (_Just . branchCommit . commitTitle . to T.unpack) $
      find (\b -> view branchName b == T.pack source) bs

getBranches ::
     (MonadUnliftIO m, MonadReader env m, HasEnv env)
  => m (Either HttpException [Branch])
getBranches =
  try $ do
    sess <- view envSession
    token <- view envAccessToken
    (T.unpack -> host, T.unpack -> project) <- getHostAndProject
    resp <-
      view Wreq.responseBody <$>
      liftIO
        (WreqS.get
           sess
           ("https://" ++
            host ++
            "/api/v4" ++
            branchesEndpointFor project ++ "?private_token=" ++ token))
    let bs = toListOf (AesonLens.values . AesonLens._JSON) resp
    return (sortByCommittedDate bs)

sortByCommittedDate :: [Branch] -> [Branch]
sortByCommittedDate =
  reverse .
  sortOn (view (branchCommit . commitCommittedDate . to zonedTimeToLocalTime))

handleHttpException ::
     (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => HttpException
  -> m ()
handleHttpException (HttpExceptionRequest _ (StatusCodeException resp content)) = do
  logError . display @Text $
    "Oops something went wrong! Any available information will be shown below."
  logError . display . T.pack $
    "Response status was: " ++ show (responseStatus resp)
  traverse_ (logError . display) (xs ++ ys)
  where
    xs =
      toListOf
        (AesonLens._JSON . AesonLens.key @Value "message" . AesonLens._String)
        content
    ys =
      toListOf
        (AesonLens._JSON .
         AesonLens.key @Value "message" . AesonLens.values . AesonLens._String)
        content
handleHttpException (HttpExceptionRequest _ (ConnectionFailure e)) = do
  logError "Oops something went wrong!"
  logError . display $ e
handleHttpException e = throw e

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = True
    , settingDisableSession = True
    , settingUseServerName = False
    }

noVerifyTlsManagerSettings :: ManagerSettings
noVerifyTlsManagerSettings = mkManagerSettings noVerifyTlsSettings Nothing
