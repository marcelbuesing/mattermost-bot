{-# LANGUAGE OverloadedStrings #-}

module MattermostBot.Data.Slack where

import Data.Aeson
import Data.Text as T

type SlackChannel = Text
type SlackUsername = Text
type SlackIconEmoji = Text

data SlackIncoming = SlackIncoming
  { _slackIncomingText :: Text
  , _slackIncomingChannel :: SlackChannel
  , _slackIncomingUsername :: SlackUsername
  , _slackIncomingIconEmoji :: SlackIconEmoji
  } deriving Show

instance ToJSON SlackIncoming where
  toJSON (SlackIncoming t c u i) =
    object
    [ "text" .= t
    , "channel" .= c
    , "username" .= u
    , "icon_emoji" .= i]
