# emacs-slack

emacs client for Slack

## dependent
- [websocket](https://github.com/ahyatt/emacs-websocket)
- [request](https://github.com/tkf/emacs-request)
- oauth2
  - do `package install`
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise
  User Interface library).
- [alert](https://github.com/jwiegley/alert)
- [emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
  - required if you want to show emoji


## configure

- get client-id and client-secret from https://api.slack.com/applications/new
- get slack-token from https://api.slack.com/web

```elisp
;; I'm using use-package and el-get

(el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-enable-emoji t) ;; if you want to enable emoji, default nil
  (setq slack-room-subscription '(test-group slackbot))
  (setq slack-client-id "hoge")
  (setq slack-client-secret "fuga")
  (setq slack-token "hogehogehoge")
  (setq slack-user-name "hogehoge"))


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

```


## how to get client-id, client-secret and slack-token

### client-id and client-secret

1. go to https://api.slack.com/applications/new
2. fill in the blanks like this
![create application](https://github.com/yuya373/emacs-slack/wiki/images/how_to_get_client_id_and_client_secret_1.png)
3. press `Create Application` and it appears
![appear client-id and client-secret](https://github.com/yuya373/emacs-slack/wiki/images/how_to_get_client_id_and_client_secret_2.png)


### slack-token

1. go to https://api.slack.com/web
2. scroll down
3. it is in the Authentication section.
![slack-token](https://github.com/yuya373/emacs-slack/wiki/images/how_to_get_api_token.png)

## how to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

- `slack-start`
  - do authorize and initialize
- `slack-ws-close`
  - turn off websoeket connection
- `slack-group-select`
  - select group from list
- `slack-im-select`
  - select direct message from list
- `slack-channel-select`
  - select channel from list
- `slack-group-list-update`
  - update group list
- `slack-im-list-update`
  - update direct message list
- `slack-channel-list-update`
  - update chennel list
- `slack-message-embed-mention`
  - use to mention to user
- `slack-message-embed-channel`
  - use to mention to channel

I'm evil user, so bind these functions if you need.


## notification

see [alert](https://github.com/jwiegley/alert).
