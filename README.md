# emacs-slack

emacs client for Slack

## [ScreenShot](https://github.com/yuya373/emacs-slack/wiki/ScreenShots)

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
- get token from https://api.slack.com/web

```elisp
;; I'm using use-package and el-get and evil

(el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-enable-emoji t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :client-id "aaaaaaaaaaa.00000000000"
   :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   :token "aaaa-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
   :subscribed-channels '(test-rename rrrrr))

  (slack-register-team
   :name "test"
   :client-id "3333333333.77777777777"
   :client-secret "cccccccccccccccccccccccccccccccc"
   :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
   :subscribed-channels '(hoge fuga))

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel)
   (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

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


### token

1. go to https://api.slack.com/web
2. scroll down
3. it is in the Authentication section.
![slack-token](https://github.com/yuya373/emacs-slack/wiki/images/how_to_get_api_token.png)

## how to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

- `slack-register-team`
  - set team configuration and create team.
  - :name, :client-id, :client-secret is needed for argumens
- `slack-change-current-team`
  - change `slack-current-team` var
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



## notification

see [alert](https://github.com/jwiegley/alert).
