# mattermost-bot
A mattermost bot, posting Gitlab commits to a configured mattermost channel.

## Setup
- Copy config template file botconfig.yaml
-Setup a Systemhook (via Administrator Panel) in Gitlab pointing to the server instance where the bot should run:
  - URL http://bot-server-instance:9666/gitlab/systemhooks
  - Secret Token: makeupyourown 
  - Check Push Events and Tag Push Events
  - Copy Secret Token to `gitlabSystemHookSecret` in your botconfig.yaml
- Setup API access to Gitlab (required because Push Events do not contain sufficient detail)
  - Create new gitlab user
  - Copy API Token in Profile -> Account -> PrivateToken to `gitlabApiPrivateToken` in your botconfig.yaml
  - Copy the Url of your Gitlab instance to `gitlabApiUrl` in your botconfig.yaml e.g. https://my-gitlab.com/
- Setup an "Incoming Webhook" in Mattermost (Integrations -> "Incoming Webhook")
  - Copy URL to `mattermostIncoming` in botconfig.yaml
- Configure target `channel` in botconfig.yaml. Notice: Lower case channel name e.g. `my-channel`!
- Copy your botconfig.yaml into the same folder as the mattermost-bot binary.
- Run the bot `./mattermost-bot`
## Tested on
Mattermost 3.7.x
Gitlab 8.16.x
