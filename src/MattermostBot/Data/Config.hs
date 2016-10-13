{-# LANGUAGE OverloadedStrings #-}
module MattermostBot.Data.Config where

import Data.Maybe (fromJust)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Network.URL (URL, importURL)

import MattermostBot.Data.Slack

data BotConfig = BotConfig
  { _botConfigChannel :: SlackChannel
  , _botConfigUsername :: SlackUsername
  , _botConfigIconEmoij :: SlackIconEmoji
  , _botConfigMattermostIncoming :: URL
  } deriving Show

instance FromJSON BotConfig where
  parseJSON (Y.Object v) =
    BotConfig <$>
    v .:   "channel" <*>
    v .:   "username" <*>
    v .:   "iconEmoij" <*>
    ((fromJust . importURL) <$> v .:   "mattermostIncoming")
  parseJSON _ = fail "Expected Object for Config value"
