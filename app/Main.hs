{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Control.Applicative ((<**>))
import Control.Lens (_Just, preview, toListOf)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, sortOn)
import Data.Time.LocalTime (ZonedTime(..))
import qualified FortyTwo
import qualified FortyTwo.Utils as FortyTwo
import qualified Githab.Api as Api
import Githab.Types
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (ManagerSettings, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import qualified Options.Applicative as OA
import RIO
import qualified RIO.Text as T
import qualified Servant.Client as ServantClient
import System.Exit (exitFailure)
import qualified Turtle

data Options = Options
  { _optionsToken :: Text
  , _optionsVerbose :: Bool
  , _optionsNoVerify :: Bool
  , _optionsTarget :: Maybe BranchName
  , _optionsAutomatic :: Bool
  , _optionsTitle :: Maybe Text
  , _optionsAutoTitle :: Bool
  }

options :: OA.Parser Options
options =
  Options <$>
  OA.option
    OA.str
    (OA.long "token" <> OA.metavar "TOKEN" <> OA.help "Access token for gitlab") <*>
  OA.switch
    (OA.long "verbose" <> OA.short 'v' <> OA.help "Enable verbose logging") <*>
  OA.switch
    (OA.long "no-verify" <>
     OA.help "Disable certificate check (on your own risk)") <*>
  optional
    (OA.option
       OA.str
       (OA.long "target" <> OA.metavar "TARGET_BRANCH" <>
        OA.help
          "Use TARGET_BRANCH as the target branch for the merge request and don't ask")) <*>
  OA.switch
    (OA.long "from-newest" <>
     OA.help "Pick the most recently commited branch as source and don't ask") <*>
  optional
    (OA.option
       OA.str
       (OA.long "title" <> OA.metavar "TITLE" <>
        OA.help "Use given title for the merge request and don't ask ")) <*>
  OA.switch
    (OA.long "auto-title" <>
     OA.help "Use the last commit as the merge request title and don't ask")

opts :: OA.ParserInfo Options
opts =
  OA.info
    (options <**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Create a merge request in gitlab")

main :: IO ()
main = do
  Options token isVerbose noVerify optTarget fromNewest maybeTitle autoTitle <-
    OA.execParser opts
  manager <-
    newManager
      (if noVerify
         then noVerifyTlsManagerSettings
         else tlsManagerSettings)
  logOptions' <- logOptionsHandle stderr isVerbose
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    let app = Env (PrivateToken token) lf manager
    runRIO app $ do
      when noVerify $ logDebug "Certificate checking is disabled!"
      eitherBranches <- fmap sortByCommittedDate <$> runClientM Api.listBranches
      case eitherBranches of
        Left e -> dieShow e
        Right bs -> do
          curBranch <- getCurrentBranch
          let bs' = toListOf (traverse . branchName) bs
          sourceBranch <-
            if fromNewest
              then case bs' of
                     [] -> dieText "No branch found..."
                     b:_ -> return b
              else if curBranch `elem` bs'
                     then fmap fromString . liftIO $
                          FortyTwo.selectWithDefault
                            "Source branch?"
                            (map branchNameToString bs')
                            (branchNameToString curBranch) <*
                          FortyTwo.flush
                     else fmap fromString . liftIO $
                          FortyTwo.select
                            "Source branch?"
                            (map branchNameToString bs') <*
                          FortyTwo.flush
          if not (null (branchNameToString sourceBranch))
            then do
              targetBranch <-
                case optTarget of
                  Just tgt -> return tgt
                  Nothing ->
                    fmap fromString . liftIO $
                    FortyTwo.selectWithDefault
                      "Target branch?"
                      (map branchNameToString bs')
                      "master" <*
                    FortyTwo.flush
              when (sourceBranch == targetBranch) $
                dieText "Source and target branch must not be equal!"
              logInfo . display . T.pack $
                "Will create: " ++
                branchNameToString sourceBranch <> " -> " <>
                branchNameToString targetBranch
              title <-
                case maybeTitle of
                  Nothing -> do
                    titlePrompt sourceBranch bs autoTitle
                  Just t -> return t
              result <-
                runClientM
                  (Api.createMergeRequest
                     (mkCreateMergeReq sourceBranch targetBranch title True))
              case result of
                Left e -> dieShow e
                Right mr -> do
                  logInfo . display $ ("...done!" :: Text)
                  logInfo . display $ view mrWebUrl mr
            else logError "No source branch given!"

shellCmd :: MonadIO m => Text -> m Text
shellCmd c = Turtle.lineToText <$> Turtle.single (Turtle.inshell c Turtle.empty)

getCurrentBranch :: MonadIO m => m BranchName
getCurrentBranch =
  liftIO . fmap BranchName $ shellCmd "git rev-parse --abbrev-ref HEAD"

getHostAndProject :: MonadIO m => m (Text, ProjectId)
getHostAndProject = do
  fullUrl <- shellCmd "git remote get-url origin"
  (host, project) <- splitRemoteOrFail fullUrl
  return (host, ProjectId project)

splitRemoteOrFail :: MonadIO m => Text -> m (Text, Text)
splitRemoteOrFail url =
  case T.split (\c -> elem @[] c "@:") url of
    [_, host, projectWithSuffix] -> return (host, T.dropEnd 4 projectWithSuffix)
    _ -> throwIO (HostExtractionException url)

titlePrompt ::
     (MonadIO m, MonadReader env m, HasLogFunc env)
  => BranchName
  -> [Branch]
  -> Bool
  -> m Text
titlePrompt source bs autoTitleEnabled =
  if autoTitleEnabled
    then case fromCommit of
           Nothing -> dieText "Could not determine title automatically"
           Just t -> return (T.pack t)
    else liftIO $
         T.pack <$>
         FortyTwo.selectWithDefault
           "Title?"
           (suggested : maybeToList fromCommit)
           defaultTitle <*
         FortyTwo.flush
  where
    defaultTitle = fromMaybe suggested fromCommit
    suggested = "Merge branch " ++ branchNameToString source
    fromCommit =
      preview (_Just . branchCommit . commitTitle . to T.unpack) $
      find (\b -> view branchName b == source) bs

sortByCommittedDate :: [Branch] -> [Branch]
sortByCommittedDate =
  reverse .
  sortOn (view (branchCommit . commitCommittedDate . to zonedTimeToLocalTime))

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = True
    , settingDisableSession = True
    , settingUseServerName = False
    }

noVerifyTlsManagerSettings :: ManagerSettings
noVerifyTlsManagerSettings = mkManagerSettings noVerifyTlsSettings Nothing

runClientM ::
     (HasEnv env, MonadIO m, MonadReader env m)
  => (PrivateToken -> ProjectId -> ServantClient.ClientM a)
  -> m (Either ServantClient.ServantError a)
runClientM action = do
  manager <- view envManager
  token <- view envAccessToken
  (T.unpack -> host, project) <- getHostAndProject
  liftIO $
    ServantClient.runClientM
      (action token project)
      (ServantClient.mkClientEnv
         manager
         (ServantClient.BaseUrl ServantClient.Https host 443 "api/v4"))

dieShow :: (HasLogFunc env, MonadReader env m, MonadIO m, Show a) => a -> m void
dieShow = die . T.pack . show

dieText :: (HasLogFunc env, MonadReader env m, MonadIO m) => Text -> m void
dieText = die

die :: (HasLogFunc env, MonadReader env m, MonadIO m, Display a) => a -> m void
die message = do
  logError . display @Text $ "...error!"
  logError . display $ message
  liftIO exitFailure
