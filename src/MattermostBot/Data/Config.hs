{-# LANGUAGE OverloadedStrings #-}
module MattermostBot.Data.Config where

import           Data.Default                (Default, def)
import           Data.Maybe                  (fromJust)
import           Data.Text as T
import qualified Data.Yaml as Y
import           Data.Yaml                   (FromJSON(..), (.:))
import           Network.URL                 (URL, importURL)

import MattermostBot.Data.Slack


type GitlabApiPrivateToken = Text

data BotConfig = BotConfig
  { _botConfigChannel :: SlackChannel
  , _botConfigUsername :: SlackUsername
  , _botConfigIconEmoij :: SlackIconEmoji
  , _botConfigMattermostIncoming :: URL
  -- | base url e.g. https://gitlab.example.com
  , _botConfigGitlabApiUrl :: URL
  , _botConfigGitlabApiPrivateToken :: GitlabApiPrivateToken
  } deriving Show

instance FromJSON BotConfig where
  parseJSON (Y.Object v) =
    BotConfig <$>
    v .:   "channel" <*>
    v .:   "username" <*>
    v .:   "iconEmoij" <*>
    ((fromJust . importURL) <$> v .:   "mattermostIncoming") <*>
    ((fromJust . importURL) <$> v .:   "gitlabApiUrl") <*>
    v .:   "gitlabApiPrivateToken"
  parseJSON _ = fail "Expected Object for Config value"

instance Default BotConfig where
  def = BotConfig "TownsSquare" "λmatterbot" ":ghost:" matterMostUrl gitlabUrl "tkn"
    where matterMostUrl = fromJust $ importURL "mattermostIncoming"
          gitlabUrl = fromJust $ importURL "https://gitlab.example.com"
