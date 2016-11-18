{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Yaml                   (decodeFile)
import           GitlabApi.Data
import           Network.Connection          (TLSSettings (..))
import           Network.HTTP.Client.TLS     (mkManagerSettings)
import           Network.HTTP.Types.Status   (ok200)
import           Network.URL                 (URL, importURL, exportURL)
import           Network.Wai.Middleware.RequestLogger
import qualified Network.Wreq as W
import           Web.Scotty

import           MattermostBot.Data

main :: IO ()
main = scotty 9666 $ do
  middleware logStdoutDev
  post "/gitlab/webhooks" $ do
    cfg <- lift botConfig
    lift $ print $ show cfg
    hdr <- header "X-Gitlab-Event"
    evt <- jsonData :: ActionM GitlabEvent
    lift $ W.postWith opts (exportURL $ _botConfigMattermostIncoming cfg) (toJSON $ whToSlack cfg evt)
    status ok200
  post "/gitlab/systemhooks" $ do
    cfg <- lift botConfig
    lift $ print $ show cfg
    hdr <- header "X-Gitlab-Event"
    evt <- jsonData :: ActionM SystemHook
    out <- lift $ toJSON <$> shToSlack cfg evt
    lift $ W.postWith opts (exportURL $ _botConfigMattermostIncoming cfg) out
    status ok200

-- | disable tls
opts = W.defaults & W.manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

botConfig :: IO BotConfig
botConfig = fromMaybe defaultCfg <$> readCfg

defaultCfg :: BotConfig
defaultCfg =
  let matterMostUrl = fromJust $ importURL "mattermostIncoming"
      gitlabUrl = fromJust $ importURL "https://gitlab.example.com"
  in BotConfig "TownsSquare" "λmatterbot" ":ghost:" matterMostUrl gitlabUrl "tkn"

readCfg :: IO (Maybe BotConfig)
readCfg = decodeFile "./botconfig.yaml"

whToSlack :: BotConfig -> GitlabEvent -> SlackIncoming
whToSlack c (WHPushEvent _ _ _ _ _ _ userName _ _ _ _ project commits _) =
  toIncoming c (
  userName <> " pushed \n"
  <> T.unlines (pushCommitToMarkDown <$> commits)
  <> "\n to " <> _projectName project)
whToSlack c (IssueEvent _ user _ _ _ _) = toIncoming c (_userUserName user <> " modified issue")
whToSlack c (PipelineEvent _ _ user project commit builds) =
  toIncoming c ("builds initiated by " <>  _userUserName user
  <> " in project " <> _pipelineEventProjectName project
  <> "\n" <> T.unlines (buildToMarkDown <$> builds))

shToSlack :: BotConfig -> SystemHook-> IO SlackIncoming
shToSlack c (SHPushEvent _ _ _ sha _ userName _ _ pid _ project commits _) = do
  cd <- commitDetails c pid sha
  let additions = T.pack $ show $ _commitStatsAdditions $ _commitSingleStats $ cd
      deletions = T.pack $ show $ _commitStatsDeletions $ _commitSingleStats $ cd
      projectUrl = "[" <> _projectName project <> "](" <> (T.pack $ exportURL $ _projectHomepage project) <> ")"
      shaUrl = "[" <> sha <> "](" <> commitHomepage project sha <> ")"
  return $ toIncoming c (
    userName <> " pushed " <> " to " <> projectUrl
    <> "\nRef:\t\t\t " <> shaUrl
    <> "\nMessage:\t " <> _commitSingleMessage cd
    <> "Stats:\t\t " <> "(+" <> additions <> " lines / -" <> deletions <> " lines)"
    <> "\nTime:\t\t " <> _commitSingleCommittedDate cd)
shToSlack c (ProjectCreated _ _ _ name _ ownerName _ pathWithNamespace id _) =
  return $ toIncoming c (":new: " <> ownerName <> " created a new project " <> name <> " " <> parenthesize  pathWithNamespace)
shToSlack c (ProjectDestroyed _ _ _ name _ ownerName _ pathWithNamespace _ _) =
  return $ toIncoming c (":x: project " <> name <> " deleted " <> parenthesize pathWithNamespace)
shToSlack c (ProjectRenamed _ _ _ name _ ownerName _ pathWithNamespace pathWithNamespaceOld _ _) =
  return $ toIncoming c (":pencil: project " <> name <> " renamed " <> parenthesize (pathWithNamespaceOld <> " -> " <> pathWithNamespace))

pushCommitToMarkDown :: Commit -> T.Text
pushCommitToMarkDown c = "- " <> "[" <> _commitMessage c <> "]" <> "(" <> T.pack (exportURL $ _commitUrl c) <> ")"

buildToMarkDown :: Build -> T.Text
buildToMarkDown b = "id: " <> T.pack (show $ _buildId b) <> " status: " <> _buildStatus b

toIncoming :: BotConfig -> T.Text -> SlackIncoming
toIncoming c t = SlackIncoming t (_botConfigChannel c) (_botConfigUsername c) (_botConfigIconEmoij c)

parenthesize :: T.Text -> T.Text
parenthesize inner = "(" <> inner <> ")"

commitHomepage :: Project -> T.Text -> T.Text
commitHomepage p sha = T.pack (exportURL $ _projectHomepage p) <> "/commit/" <> sha

commitDetails :: BotConfig -> ProjectId -> CommitRef -> IO CommitSingle
commitDetails c pid cr = do
  resp <- let gitlabApi = _botConfigGitlabApiUrl c
              url =  (exportURL gitlabApi) <> "/api/v3/projects/" <> (show pid) <> "/repository/commits/" <> (T.unpack cr)
              tkn = encodeUtf8 (_botConfigGitlabApiPrivateToken c)
              opt = W.defaults & W.header "PRIVATE-TOKEN" .~ [tkn]
          in W.asJSON =<< W.getWith opt url :: IO (W.Response CommitSingle)
  return $ resp ^. W.responseBody
